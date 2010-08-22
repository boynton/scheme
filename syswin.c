/* LeeScheme/syswin.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include "sys.h"
#include <winsock2.h>
#include <windows.h>
#include <stdio.h>
#include <signal.h>
#include <sys/stat.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>

#define S_ISREG(n) (((n) & S_IFMT) == S_IFREG)

#define SCM_DIR "~/lib/scheme"
#define BOOT_FILE "scheme.scm"
#define REPL_FILE "repl.scm"

//#define HEAPSIZE 2048
#define HEAPSIZE 32768
#define STACKSIZE 64
#define CODESIZE 128

static char *home = "";
static DWORD launch_time;

static int interrupts_enabled = 1;
object stdin_port, stdout_port, stderr_port;

int interrupt_char = 3;


static void beep(void) {
	putc(CHAR_BEEP,stdout);
	fflush(stdout);
}

long msec_time_stamp(void) {
	return GetTickCount() - launch_time;
}

int scheme_interrupt_request = 0;
int scheme_key_irq = 0;
int scheme_inside_getc = 0;

void keyboard_interrupt(int n) {
	if (scheme_key_irq) {
		signal(SIGINT,&keyboard_interrupt);
		scheme_interrupt_request = 1;
	}
}

static char **main_argv;
static int main_argc;

static int timing_info = 0;
void main(int argc, char *argv[]) {
	char *s;
	main_argv = argv;
	main_argc = argc;
	if (argc > 1) {
		s = argv[1];
		timing_info = 1;
	} else
		s = REPL_FILE;
	launch_time = GetTickCount();
	init_scheme(HEAPSIZE,STACKSIZE,CODESIZE);
	define("system:*search-path*",list1(make_string(SCM_DIR)));
	define("system:*repl-filename*",make_string(s));
	execute_file(BOOT_FILE);
	quit(0);
}

extern int gc_count;
extern void quit(long exit_code) {
	long msec = msec_time_stamp();
	if (timing_info) {
		printf("\nExecution took %ld.%2ld seconds\n",msec/1000,msec%1000);
                printf("Garbage collected %d times\n", gc_count);
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
	Sleep(msec);
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
	if (scheme_interrupt_request) {
		scheme_interrupt_request = 0;
		interrupt();
	}
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
    //poll the console input for a ctrl-c
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
	WIN32_FIND_DATA fileData;
	HANDLE hDir;
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
	hDir = FindFirstFile(p, &fileData);
	if (hDir != INVALID_HANDLE_VALUE) {
		object o;
		PUSH_GC_PROTECT(o);
		do {
			object o = make_string(fileData.cFileName);
			*sp = cons(o, *sp);
		} while (FindNextFile(hDir, &fileData));
		FindClose(hDir);
		POP_GC_PROTECT(1);
	}
}

int serial_open(char *device, int baud) {
	static DCB dcb;
	BOOL b;
	HANDLE hCom = CreateFile("COM2",
			GENERIC_READ|GENERIC_WRITE,
			0, /* exclusive */
			NULL, /* no security */
			OPEN_EXISTING,
			0, /* Not overlapped I/O */
			NULL);
	if (hCom == INVALID_HANDLE_VALUE)
		return -1;
	b = GetCommState(hCom, &dcb);
	if (!b) {
		CloseHandle(hCom);
		return -2;
	}
	dcb.BaudRate = baud;
	dcb.ByteSize = 8;
	dcb.Parity = NOPARITY;
	dcb.StopBits = ONESTOPBIT;
	b = SetCommState(hCom, &dcb);
	if (!b) {
		CloseHandle(hCom);
		return -3;
	}
	return (int)hCom;
}


static int serial_write(int hCom, char *p, int n) {
	DWORD i;
	BOOL b = WriteFile((HANDLE)hCom, p, n, &i, NULL);
	if (b)
		return i;
	else
		return -1;
}
static void serial_port_putc(object p, int c) {
	char buf[1] = {(char)c};
	int handle = (int)PORT_STREAM(p);
	check_interrupts();
	if (serial_write(handle,buf,1) != 1)
		error(p, "Cannot write serial port");
}

static void serial_close(int hCom) {
	CloseHandle((HANDLE)hCom);
}
static void serial_port_close(object p) {
	int handle = (int)PORT_STREAM(p);
	serial_close(handle);
}

static void primop_serial_output_port(long argc) {
	/* "COM2", 38400 */
	char *device;
	int baud, handle;
	TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
	TYPE_CHECK(FIXNUM_P(sp[1]),2,"fixnum",sp[1]);
	device = STRING_VALUE(*sp++);
	baud = FIXNUM_VALUE(*sp);
	handle = serial_open(device, baud);
	if (handle >= 0) {
		object p = make_port((void *)handle,PORT_FLAG_WRITE,device);
		PORT_CLOSE_PROC(p) = &serial_port_close;
		PORT_GETC_PROC(p) = NULL;
		PORT_PUTC_PROC(p) = &serial_port_putc;
		PORT_FLUSH_PROC(p) = NULL;
		PORT_READY_PROC(p) = NULL;
		*sp = p;
	} else {
		error(make_string(device),"Cannot open serial port");
	}
}

static HMIDIOUT hMidiOut = 0;

static void primop_midi_open(long argc) {
    UINT result;
    result = midiOutOpen(&hMidiOut, MIDI_MAPPER, 0, 0, CALLBACK_NULL);
    if (result != MMSYSERR_NOERROR)
		*--sp = false_object;
	else
		*--sp = true_object;
}

static void primop_midi_close(long argc) {
	if (hMidiOut) {
		UINT result = midiOutClose(hMidiOut);
		hMidiOut = 0;
	}
	*--sp = true_object;
}

static void primop_midi_write(long argc) {
    DWORD foo;
    MMRESULT res;
	int a, b = 0, c = 0;
	object n = *sp++;
	if (!FIXNUM_P(n)) {
		error(n,"argument 1 is not an integer");
	}
	a = FIXNUM_VALUE(n);
	if (argc > 1) {
		object n = *sp++;
		if (!FIXNUM_P(n)) {
			error(n,"argument 2 is not an integer");
		}
		b = FIXNUM_VALUE(n);
		if (argc > 2) {
			object n = *sp++;
			if (!FIXNUM_P(n)) {
				error(n,"argument 3 is not an integer");
			}
			c = FIXNUM_VALUE(n);
		}
	}
    foo = (a & 0xff) | ((b & 0xff) << 8) | ((c & 0xff) << 16);
    res = midiOutShortMsg(hMidiOut, foo);
	*--sp = true_object;
}

int sys_socket_connect(char *szHost, int nPort) {
	int sockfd;
	struct hostent *hp;
	struct sockaddr_in serv_addr;
	memset((char *)&serv_addr, 0, sizeof(serv_addr));
	//if (!sys_socket_init())
	//      return -1;
	hp = gethostbyname(szHost);
	if (hp)
		memcpy((char *) &serv_addr.sin_addr, hp->h_addr, hp->h_length);
	else {
		int err = WSAGetLastError();
		//printf("last error = %d\n", err);
		serv_addr.sin_addr.s_addr = inet_addr(szHost);
	}
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons((short)nPort);
	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) >= 0) {
		if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) >= 0\
			) {
			return sockfd;
		} else {
			int n = WSAGetLastError();
			//printf("Socket error: %d\n", n);
		}
		closesocket(sockfd);
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
	int handle = (int)PORT_STREAM(p);
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
	u_long count;
	int handle = (int)PORT_STREAM(p);
	return (ioctlsocket(handle, FIONREAD, &count) == 0 && count != 0);
}

int sys_socket_write(int hSocket, char *buf, int nBytes) {
        return send(hSocket, buf, nBytes, 0);
}
static void socket_port_putc(object p, int c) {
	//buffer this to optimize!
	char buf[2] = {(char)c, 0};
	int len = 1;
	int handle = (int)PORT_STREAM(p);
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
	//?
}

void sys_socket_shutdown(int hSocket) {
	shutdown(hSocket, SD_SEND);
}

static void socket_port_close(object p) {
	int handle = (int)PORT_STREAM(p);
	closesocket(handle);
}

static object make_socket_port(int handle, char *host, int port) {
	char tmp[256];
	object p;
	object s = null_object;
	if (host)
		sprintf(tmp, "%s:%d", host, port);
	else
		sprintf(tmp, "Client %d", handle);
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
		*--sp = make_socket_port(handle, host, port);
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
        closesocket(sock);
        return -1;
    }
    
    myname.sin_family = AF_INET;  /* Internet address */
    myname.sin_port = htons((u_short)port);
	myname.sin_addr.s_addr = INADDR_ANY;  /* "Wildcard" */
    memset(&(myname.sin_zero), 0, 8);
    
    if (bind(sock, (const struct sockaddr *)&myname, sizeof(myname) ) < 0) {
        closesocket(sock);
        return -1;
    }
    
    if (listen ( sock, 5 ) < 0) {
        closesocket (sock);
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
		*--sp = make_socket_port(client, NULL, -1);
	} else
		error(p, "Cannot accept connection");
}


extern void init_system(void) {
	WSADATA wsaData;
	home = getenv("HOME");
	if (!home) home = "";
	WSAStartup(MAKEWORD(2, 0),&wsaData);
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
	define_primop("system:open-serial-output",primop_serial_output_port,2,2);
	define_primop("midi:open",primop_midi_open, 0, 0);
	define_primop("midi:close",primop_midi_close, 0, 0);
	define_primop("midi:write",primop_midi_write, 1, 3);
	define_primop("system:connect",primop_connect, 1, 2);
	define_primop("system:listen",primop_listen, 1, 1);
	define_primop("system:accept",primop_accept, 1, 1);
//	define_primop("system:close-socket",primop_close_socket, 1, 1);
}
