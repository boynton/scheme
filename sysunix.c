/* LeeScheme/sysunix.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include "sys.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <pwd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
/*#include <pthread.h>*/

#ifdef OPENBSD
#include <dirent.h>
#include <unistd.h>
#include <sys/wait.h>
#endif

#ifdef APPLE
#include <dirent.h>
#include <unistd.h>
#include <sys/wait.h>
#endif

#ifdef LINUX
#include <dirent.h>
#include <unistd.h>
#include <sys/wait.h>
#ifdef ALSA
#include <alsa/asoundlib.h>
#else
#include <linux/soundcard.h>
#endif
#include <sys/ioctl.h>
#endif

#ifdef SUNOS4X
#include <termio.h>
char *strerror(int i) {
	extern char *sys_errlist[];
	return sys_errlist[i];
}
#endif /* SUNOS4X */

#ifdef SOLARIS
#include <dirent.h>
#include <sgtty.h>
#endif /* SOLARIS */

#ifdef NeXT
#include <sys/ioctl.h>
#define S_ISREG(mode)	(((mode) & (S_IFMT)) == (S_IFREG))
#endif /* NeXT */

#ifdef APPLE
#define SCM_DIR "/Users/lee/lib/scheme"
#else
#define SCM_DIR "/home/lee/lib/scheme"
#endif
#define BOOT_FILE "scheme.scm"
#define REPL_FILE "repl.scm"

//#define HEAPSIZE 32768
#define HEAPSIZE 100000
#define STACKSIZE 64
#define CODESIZE 128

static char *home = "";
static struct timeval launch_time;

static int interrupts_enabled = 1;
object stdin_port, stdout_port, stderr_port;

int interrupt_char = 3;


static void beep(void) {
	putc(CHAR_BEEP,stdout);
	fflush(stdout);
}

long msec_time_stamp(void) {
	struct timeval tv;
	long deltausec, deltasec;
	gettimeofday(&tv,NULL);
	deltausec = tv.tv_usec - launch_time.tv_usec;
	deltasec = tv.tv_sec - launch_time.tv_sec;
	if (deltausec < 0) {
		deltausec += 1000000;
		deltasec--;
	}
	return deltasec * 1000 + deltausec / 1000;
}

int scheme_interrupt_request = 0;
int scheme_key_irq = 0;
int scheme_inside_getc = 0;

void keyboard_interrupt(int n) {
	if (scheme_key_irq) {
		signal(SIGINT,&keyboard_interrupt);
		if (scheme_inside_getc) {
			scheme_interrupt_request = 0;
			interrupt();
		} else
			scheme_interrupt_request = 1;
	}
}

static char **main_argv;
static int main_argc;

static int timing_info = 0;
int main(int argc, char *argv[]) {
	char *s;
	main_argv = argv;
	main_argc = argc;
	if (argc >= 2 && !strcmp(argv[1], "-t")) {
		timing_info = 1;
		s = argv[2];
	} else if (argc > 1) {
		s = argv[1];
	} else
		s = REPL_FILE;
	gettimeofday(&launch_time,NULL);
	init_scheme(HEAPSIZE,STACKSIZE,CODESIZE);
	define("system:*search-path*",list1(make_string(SCM_DIR)));
	define("system:*repl-filename*",make_string(s));
	execute_file(BOOT_FILE);
	quit(0);
	return 0;
}

extern void quit(long exit_code) {
    long msec = msec_time_stamp();
    if (timing_info) {
	printf("\nExecution took %ld.%2ld seconds\n",msec/1000,msec%1000);
#ifdef INSTRUMENT_OPS
	if (1) {
	    extern int opcount[];
	    int i;
	    for (i=0; i<=OPCODE_APPLY; i++) {
		printf("%d\t%s\n", opcount[i], opcode_name(MAKE_OPCODE(i)));
	    }
	}
#endif
    }
    /* close all open files */
    exit((int)exit_code);
}

int enable_interrupts(int new) {
	int old = interrupts_enabled;
	interrupts_enabled = new;
	return old;
}

int execute_system_command(char *the_command) {
	return system(the_command)? 0 : 1;
}

void *allocate_memory(long size) {
	void *s = malloc(size);
	if ((long)s & 3) fatal_error("Malloc return: %x\n",s);
	return s;
}

void free_memory(void *ptr) {
	free(ptr);
}

int does_file_exist(char *name) {
	struct stat sb;
	int fd = open(name,O_RDONLY,0);
	if (fd < 0) return 0;
	if (fstat(fd,&sb) || !S_ISREG(sb.st_mode)) {
		close(fd);
		return 0;
	} else {
		close(fd);
		return 2;
	}
}

void expand_path(char *buf,char *file) {
	if (file[0] == '~') {
		if (!file[1]) {
			strcpy(buf,home);
			return;
		} else if (file[1] == '/') {
			sprintf(buf,"%s/%s", home, file+2);
			return;
		} else {
			struct passwd *user;
			char *q = strchr(file,'/');
			if (q) {
				*q++ = '\0';
				user = getpwnam(file);
				if (user) {
					sprintf(buf,"%s/%s", user->pw_dir, q);
					return;
				}
			}
		}
	}
	strcpy(buf,file);
}

extern void remove_file(char *name) {
	if (remove(name)) 
		error(make_string(name),strerror(errno));
}

void get_home_directory(char *buffer) {
	strcpy(buffer,home);
}

long msec_since_launch(void) {
	return msec_time_stamp();
}

void msec_sleep(long msec) {
	double seconds = msec * 0.001;
	long integer_usec = (long)(seconds * 1000000.0);
	long integer_sec = (long)(seconds);
	struct timeval timeout;
	timeout.tv_sec = integer_sec;
	timeout.tv_usec = integer_usec;
	select(0,0,0,0,&timeout);
}


/* File port support */

static void file_port_close(object p) {
	FILE *s = PORT_STREAM(p);
	if (s) {
		fclose(s);
		PORT_STREAM(p) = NULL;
	}
	PORT_FLAGS(p) |= PORT_FLAG_CLOSED;
}

static int file_port_getc(object p) {
	FILE *s = PORT_STREAM(p);
	int c;
	scheme_inside_getc = 1;
	c = getc(s);
	scheme_inside_getc = 0;
	return c;
}

static void file_port_putc(object p, int c) {
	FILE *s = PORT_STREAM(p);
	check_interrupts();
	putc(c,s);
}

static void file_port_flush(object p) {
	FILE *s = PORT_STREAM(p);
	fflush(s);
}

static int file_port_ready_p(object p) {
	return 1;
}

object open_input_file(char *filename) {
	char fullpath[MAX_FILENAME_LENGTH];
	FILE *s = NULL;
	object p;
	if (find_file(fullpath,filename)) {
		s = fopen(fullpath,"r");
	}
	if (!s)
		error(unbound_object,"File not found: %s", filename);
	p = make_port(s,PORT_FLAG_READ,fullpath);
	PORT_CLOSE_PROC(p) = &file_port_close;
	PORT_GETC_PROC(p) = &file_port_getc;
	PORT_PUTC_PROC(p) = &file_port_putc;
	PORT_FLUSH_PROC(p) = &file_port_flush;
	PORT_READY_PROC(p) = &file_port_ready_p;
	return p;
}

object open_output_file(char *path) {
	FILE *s = fopen(path,"w");
	object p;
	if (!s)
		error(unbound_object,"Cannot write file: %s", path);
	p = make_port(s,PORT_FLAG_WRITE,path);
	PORT_CLOSE_PROC(p) = &file_port_close;
	PORT_GETC_PROC(p) = &file_port_getc;
	PORT_PUTC_PROC(p) = &file_port_putc;
	PORT_FLUSH_PROC(p) = &file_port_flush;
	PORT_READY_PROC(p) = &file_port_ready_p;
	return p;
}

char *newline_string(object the_port) {
	return "\n";
}

void check_interrupts() {
	if (interrupts_enabled && scheme_interrupt_request) {
		scheme_interrupt_request = 0;
		interrupt();
	}
}


int printable_char_p(char c) {
	/* Hack alert! */
	if (c >= 0 && c < ' ') return 0;
	if (c == 127) return 0;
	return 1;
}

int usable_control_char_p(char c) {
	if (c == '\n' || c == '\t' || c == CHAR_BACKSPACE || c == '\r')
		return 1;
	return 0;
}

static void primop_stdin_port(long argc) {
	if (FALSE_P(stdin_port)) {
		object p = make_port((void *)stdin,PORT_FLAG_READ,"stdin");
		PORT_CLOSE_PROC(p) = &file_port_close;
		PORT_GETC_PROC(p) = &file_port_getc;
		PORT_PUTC_PROC(p) = NULL;
		PORT_FLUSH_PROC(p) = NULL;
		PORT_READY_PROC(p) = &file_port_ready_p;
		stdin_port = p;
	}
	*--sp = stdin_port;
}

static void primop_stdout_port(long argc) {
	if (FALSE_P(stdout_port)) {
		object p = make_port((void *)stdout,PORT_FLAG_WRITE,"stdout");
		PORT_CLOSE_PROC(p) = &file_port_close;
		PORT_GETC_PROC(p) = NULL;
		PORT_PUTC_PROC(p) = &file_port_putc;
		PORT_FLUSH_PROC(p) = &file_port_flush;
		PORT_READY_PROC(p) = NULL;
		stdout_port = p;
	}
	*--sp = stdout_port;
}

static void primop_stderr_port(long argc) {
	if (FALSE_P(stderr_port)) {
		object p = make_port((void *)stderr,PORT_FLAG_WRITE,"stderr");
		PORT_CLOSE_PROC(p) = &file_port_close;
		PORT_GETC_PROC(p) = NULL;
		PORT_PUTC_PROC(p) = &file_port_putc;
		PORT_FLUSH_PROC(p) = &file_port_flush;
		PORT_READY_PROC(p) = NULL;
		stderr_port = p;
	}
	*--sp = stderr_port;
}

/* primitives */

static void primop_argv(long argc) {
	object argv = null_object;
	int i = main_argc;
	PUSH_GC_PROTECT(argv);
	while (i--) argv = cons(make_string(main_argv[i]),argv);
	*--sp = argv;
	POP_GC_PROTECT(1);
}

static void primop_beep(long argc) {
	beep();
	*--sp = void_object;
}

static void primop_key_irq(long argc) {
	if (FALSE_P(*sp)) {
		scheme_key_irq = 0;
	} else {
		scheme_key_irq = 1;
		signal(SIGINT,&keyboard_interrupt);
	}
}

static void primop_host_os(long argc) {
	*--sp = intern("unix");
}

static void primop_pwd(long argc) {
	char buf[1024];
	getcwd(buf,sizeof(buf));
	*--sp = make_string(buf);
}

static void primop_chdir(long argc) {
	TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
	chdir(STRING_VALUE(sp[0]));
}

static void primop_file_listing(long argc) {
	DIR *hDir;
	char buf[1024];
	char *p;
	if (argc == 1) {
		TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
		p = STRING_VALUE(sp[0]);
	} else {
		getcwd(buf,sizeof(buf));
		p = buf;
	}
	*--sp = null_object;
	hDir = opendir(p);
	if (hDir) {
		object o;
		struct dirent *pInfo = readdir(hDir);
		PUSH_GC_PROTECT(o);
		while (pInfo) {
			object o = make_string(pInfo->d_name);
			*sp = cons(o, *sp);
			pInfo = readdir(hDir);
		}
		closedir(hDir);
		POP_GC_PROTECT(1);
	}
}

#ifdef MIDI
#ifdef LINUX

#ifdef ALSA

snd_rawmidi_t *midi_out = NULL;
int midi_out_fd = -1;

static void primop_midi_open(long argc) {
#if 1
    /* midi_out_fd = open("/dev/midi", O_WRONLY); */
    midi_out_fd = open("/dev/snd/midiC0D0", O_WRONLY);
    if (midi_out_fd < 0) {
	printf("snd_rawmidi_open failed: %d\n", errno);
	*--sp = false_object;
    } else {
	printf("midi_open: %d\n", midi_out_fd);
	*--sp = true_object;
    }
#else
    int err = snd_rawmidi_open(NULL, &midi_out, "/dev/midi00", 0);
    if (err) {
	printf("snd_rawmidi_open failed: %d\n", err);
	*--sp = false_object;
    } else {
	printf("midi_open!\n");
	*--sp = true_object;
    }
#endif
}

static void primop_midi_close(long argc) {
    printf("midi_close %d\n", midi_out_fd);
#if 1
    if (midi_out_fd >= 0) {
	close(midi_out_fd);
	midi_out_fd = -1;
    }
#else
    if (midi_out) {
	snd_rawmidi_close(midi_out);
	midi_out = NULL;
    }
#endif
}

static void primop_midi_write(long argc) {
    int i, n, err;
    unsigned char buf[128];
    printf("midi_write %d [%d]\n", midi_out_fd, argc);
    if (midi_out_fd < 0) {
	printf("midi isn't open\n");
	error(null_object,"midi isn't open");
    }
    if (argc > 128) {
	printf("too much data\n");
	error(null_object, "too much data for midi_write");
    }
    for (i=0; i<argc; i++) {
	n = *sp++;
	if (!FIXNUM_P(n)) {
	    error(n,"argument 1 is not an integer");
	}
	buf[i] = (char)FIXNUM_VALUE(n);
	printf("write %02x\n", buf[i]);
    }
#if 1
    i = write(midi_out_fd, buf, argc);
    if (i != argc) {
	printf("snd_rawmidi_write failed: %d\n", err);
	*--sp = false_object;
    } else 
	*--sp = true_object;
#else
    err = snd_rawmidi_write(midi_out, buf, argc);
    if (err) {
	printf("snd_rawmidi_write failed: %d\n", err);
	*--sp = false_object;
    } else 
	*--sp = true_object;
#endif
}

#else
/* linux kernal OSS stuff */
static int seqfd = -1;

static void primop_midi_open(long argc) {
  /*
  for now, just synth. Note: midi external is trivial:
   1. open /dev/midi
   2. write raw midi bytes
  For synth, we have to pack things wierdly.
  */
  int status, numMidi;
  seqfd = open("/dev/sequencer", O_WRONLY, 0);
  if (seqfd >= 0) {
    status = ioctl(seqfd, SNDCTL_SEQ_NRMIDIS, &numMidi);
    if (status != 0) {
      printf("Cannot query midi\n");
      close(seqfd);
      seqfd = -1;
      *--sp = false_object;
    }
    if (numMidi <= 0) {
      printf("no midi devices\n");
      close(seqfd);
      seqfd = -1;
      *--sp = false_object;
    } else {
      printf("midi_open[%d]\n", seqfd);
      *--sp = true_object;
    }
    
  } else {
    printf("Cannot open midi\n");
    *--sp = false_object;
  }
}

static void primop_midi_close(long argc) {
  if (seqfd >= 0) {
    close(seqfd);
    printf("midi_close[%d]\n",seqfd);
    seqfd = -1;
    *--sp = true_object;
  } else
    *--sp = false_object;
}

static void primop_midi_write(long argc) {
  int device = 0;
  unsigned char outpacket[12];
  int outcount = 4;
  object n;

  if (seqfd < 0) {
    error(null_object,"midi isn't open");
  }
  n = *sp++;
  if (!FIXNUM_P(n)) {
    error(n,"argument 1 is not an integer");
  }
  outpacket[0] = SEQ_MIDIPUTC;
  outpacket[1] = FIXNUM_VALUE(n);
  outpacket[2] = device;
  outpacket[3] = 0;
  
  if (argc > 1) {
    n = *sp++;
    if (!FIXNUM_P(n)) {
      error(n,"argument 2 is not an integer");
    }
    outcount += 4;
    outpacket[4] = SEQ_MIDIPUTC;
    outpacket[5] = FIXNUM_VALUE(n);
    outpacket[6] = device;
    outpacket[7] = 0;
    if (argc > 2) {
      n = *sp++;
      if (!FIXNUM_P(n)) {
	error(n,"argument 3 is not an integer");
      }
      outcount += 4;
      outpacket[8] = SEQ_MIDIPUTC;
      outpacket[9] = FIXNUM_VALUE(n);
      outpacket[10] = device;
      outpacket[11] = 0;
    }
  }
  write(seqfd, outpacket, outcount);
  if (argc == 1)
      printf("midiwrite[%d]: %02x\n", seqfd, outpacket[1]);
  else if (argc == 2)
      printf("midiwrite[%d]: %02x %02x\n", seqfd, outpacket[1], outpacket[5]);
  else if (argc == 3)
      printf("midiwrite[%d]: %02x %02x %02x\n", seqfd, outpacket[1], outpacket[5], outpacket[9]);
  *--sp = true_object;
}

#endif /* !ALSA */
#endif /* LINUX */
#endif /* MIDI */

int sys_socket_connect(char *szHost, int nPort) {
    int sockfd;
    struct hostent *hp;
    struct sockaddr_in serv_addr;
    memset((char *)&serv_addr, 0, sizeof(serv_addr));
    hp = gethostbyname(szHost);
    if (hp)
	memcpy((char *) &serv_addr.sin_addr, hp->h_addr, hp->h_length);
    else {
	serv_addr.sin_addr.s_addr = inet_addr(szHost);
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons((short)nPort);
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) >= 0) {
	if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) >= 0) {
	    return sockfd;
	} else {
	}
	close(sockfd);
    }
    return -1;
}

int sys_socket_read(int hSocket, char *buf, int nBytes) {
    int nRemaining = nBytes;
    char *p = buf;
    while (nRemaining > 0) {
	int n = recv(hSocket, p, nRemaining, 0);
	if (n <= 0)
	    return (nBytes - nRemaining);
	nRemaining -= n;
	p += n;
    }
    return nBytes;
}

static int socket_port_getc(object p) {
    int handle = (int)((long)PORT_STREAM(p));
    int c;
    char buf[1];
    scheme_inside_getc = 1;
    if (1 != recv(handle, buf, 1, 0))
	c = -1;
    else
	c = buf[0];
    scheme_inside_getc = 0;
    if (scheme_interrupt_request) {
	scheme_interrupt_request = 0;
	interrupt();
    }
    return c;
}

static int socket_port_ready_p(object p) {
    /*
    u_long count;
    int handle = (int)PORT_STREAM(p);
    return (ioctlsocket(handle, FIONREAD, &count) == 0 && count != 0);
    */
    return 1;
}

int sys_socket_write(void *hSocket, char *buf, int nBytes) {
    int h = (int)((long)hSocket);
    return send(h, buf, nBytes, 0);
}
static void socket_port_putc(object p, int c) {
    char buf[2] = {(char)c, 0};
    int len = 1;
    void * handle = (void *)PORT_STREAM(p);
    check_interrupts();
    if ((PORT_FLAGS(p) & PORT_FLAG_COOKED) && c == 10) {
	buf[0] = '\r';
	buf[1] = '\n';
	len = 2;
    }
    if (sys_socket_write(handle,buf,len) != len)
	error(p, "Cannot write socket");
}
static void socket_port_flush(object p) {
}

void sys_socket_shutdown(int hSocket) {
    shutdown(hSocket, 1);
}

static void socket_port_close(object p) {
    int handle = (int)(long)PORT_STREAM(p);
    close(handle);
    PORT_FLAGS(p) |= PORT_FLAG_CLOSED;
    close_stale_port(p);
}

static object make_socket_port(void * handle, char *host, int port) {
    char tmp[256];
    object p;
    if (host)
	sprintf(tmp, "%s:%d", host, port);
    else
	sprintf(tmp, "Client %p", handle);
    p = make_port((void *)handle,PORT_FLAG_READ|PORT_FLAG_WRITE,tmp);
    PORT_CLOSE_PROC(p) = &socket_port_close;
    PORT_GETC_PROC(p) = &socket_port_getc;
    PORT_PUTC_PROC(p) = &socket_port_putc;
    PORT_FLUSH_PROC(p) = &socket_port_flush;
    PORT_READY_PROC(p) = &socket_port_ready_p;
    return p;
}

static void primop_connect(long argc) {
	char *host;
	int port=80, handle;
	TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
	host = STRING_VALUE(*sp++);
	if (argc == 2) {
		TYPE_CHECK(FIXNUM_P(sp[1]),2,"fixnum",sp[1]);
		port = FIXNUM_VALUE(*sp++);
	}
	handle = sys_socket_connect(host, port);
	if (handle >= 0) {
            *--sp = make_socket_port((void *)(long)handle, host, port);
	} else {
		error(make_string(host),"Cannot connect to host");
	}
}

int sys_socket_listen(int port) {
    struct sockaddr_in myname;  /* Internet socket name */
    int sock;
    int i;
    
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        return -1;

    i = 1;
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char *)&i, sizeof(i)) < 0) {
        close(sock);
        return -1;
    }
    
    myname.sin_family = AF_INET;  /* Internet address */
    myname.sin_port = htons((unsigned short)port);
	myname.sin_addr.s_addr = INADDR_ANY;  /* "Wildcard" */
    memset(&(myname.sin_zero), 0, 8);
    
    if (bind(sock, (const struct sockaddr *)&myname, sizeof(myname) ) < 0) {
        close(sock);
        return -1;
    }
    
    if (listen ( sock, 5 ) < 0) {
        close(sock);
        return -1;
    }
    return sock;
}

static void primop_listen(long argc) {
	int port;
	int handle;
	object p = *sp++;
	TYPE_CHECK(FIXNUM_P(p),1,"fixnum",p);
	port = FIXNUM_VALUE(p);
	handle = sys_socket_listen(port);
	if (handle < 0)
		error(p, "Cannot listen on port");
	*--sp = MAKE_FIXNUM(handle);
}

int sys_socket_accept(int server) {
    int sock = accept(server, 0, 0);
    if (0 && sock >= 0) {
	int b = 1;
	setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (const char *)&b, sizeof(b));
    }
    return sock;
}

static void primop_accept(long argc) {
	int handle, client;
	object p = *sp++;
	TYPE_CHECK(FIXNUM_P(p),1,"fixnum",p);
	handle = FIXNUM_VALUE(p);
	client = sys_socket_accept(handle);
	if (client >= 0) {
            *--sp = make_socket_port((void *)(long)client, NULL, -1);
	} else
		error(p, "Cannot accept connection");
}

static void primop_socket_close(long argc) {
	object tmp = *sp;
	TYPE_CHECK(PORT_P(tmp),1,"socket-port",tmp);
	PORT_CLOSE_PROC(tmp)(tmp);
}

#if 0
static void primop_socket_close(long argc) {
	int handle;
	object p = *sp++;
	TYPE_CHECK(PORT_P(p),1,"port",p);
	//TYPE_CHECK(FIXNUM_P(p),1,"fixnum",p);
	//handle = FIXNUM_VALUE(p);
	handle = (int)PORT_STREAM(p);
	sys_socket_close(handle);
	*--sp = true_object;
}
#endif

extern void init_system(void) {
	home = getenv("HOME");
	if (!home) home = "";
	define("system:*tab-size*",MAKE_FIXNUM(8));
	define_primop("system:host-os",primop_host_os,0,0);
	define_primop("system:pwd",primop_pwd,0,0);
	define_primop("system:chdir",primop_chdir,1,1);
	define_primop("system:file-listing",primop_file_listing,0,1);
	define_primop("system:beep",primop_beep,0,0);
	stdin_port = false_object;
	PUSH_GC_PROTECT(stdin_port);
	stdout_port = false_object;
	PUSH_GC_PROTECT(stdout_port);
	stderr_port = false_object;
	PUSH_GC_PROTECT(stderr_port);
	define_primop("system:enable-keyboard-interrupts",primop_key_irq,1,1);
	define_primop("system:stdin",primop_stdin_port,0,0);
	define_primop("system:stdout",primop_stdout_port,0,0);
	define_primop("system:stderr",primop_stderr_port,0,0);
	define_primop("system:argv",primop_argv,0,0);
#ifdef MIDI
	define_primop("midi:open",primop_midi_open, 0, 0);
	define_primop("midi:close",primop_midi_close, 0, 0);
	define_primop("midi:write",primop_midi_write, 1, 3);
#endif
	define_primop("system:connect",primop_connect, 1, 2);
	define_primop("system:listen",primop_listen, 1, 1);
	define_primop("system:accept",primop_accept, 1, 1);
	define_primop("system:socket-close",primop_socket_close, 1, 1);
}





