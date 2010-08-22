;; LeeScheme/bench.scm. Copyright (C) Lee Richard Boynton, 1993-2000.

;
;A simple integer performance metric.
;
;The following results assume a 2M heap, everything on local disk,
;and is the total time elapsed in seconds of loading, compiling,
;and running the benchmark. Scheme-4.3 was used.
;
; (NOTE: this benchmark doesn't track the increasing available memory well. The 486 DX2-40 had 8MB of RAM,
;  so this benchmark was set to use 1/4 of that just for the heap.
;  Now, the Athlon XP2400 has 1GB of RAM, so a comparable "cost" memory setting would be a 256M heap (not 2 MB!)
;  Setting the heap to 32MB means no gc's occur anyway. This saves about 16% on the execution time. So, when the heap
;  is set such that 23 gc's occur, a 16% overhead is measured)
;
; ! On 6/16/2004, I measured the AMD 2400 with a 32MB heap, and it was exactly 100 times faster than the Tosh 1950 (1992).
; ! That is: 100x in 12 years.
;
;**** NEW: change the benchmark to sort 30000 numbers instead of
; 10000, and the second pi calculation is now to 518 digits in groups
; of 4, not 200 digits in groups of 5. This scales the number back up
; into something reasonable. All tests now assume bind-primops is true,
; but full type checking and argument count checking is enabled.
;
; *** Optimized C implementation
; Intel Core i7 2.80GHz/OSX10.6.2               0.14   * leescheme-5.9, 64 bit executable, xcode 3.2.1, -O3 -fomit-frame-pointer -arch x86_64 -DX86_64
; AMD 1138/RH73/Bigloo2.5a -O6                  0.18   * 10k executable
; Intel Core 2 Duo2.16GHz/OSX10.4.8/gcc4.0.1    0.24   * leescheme-5.9, 32 bit executable, xcode -O3, MacBookPro v2 2.16GHz
; Intel Core 2 Duo2.00GHz/OSX10.6               0.25   * leescheme-5.9, 64 bit executable, ClangLLVM v1.0 -O3, MacMini v3 2GHz DDR3
; Intel Core 2 Duo2.00GHz/OSX10.6               0.26   * leescheme-5.9, 64 bit executable, gcc4.2.1 -O3, MacMini v3 2GHz DDR3
; Intel Core 2 Duo2.00GHz/OSX10.5.1             0.27   * leescheme-5.9, 32 bit executable, xcode -O3, MacBook v4 2GHz
; Intel Core 2 Duo2.00GHz/OSX10.5.6             0.28   * leescheme-5.9, 64 bit executable, xcode -O3, MacMini v3 2GHz no DDR
; Intel Core 2 Duo2.16GHz/OSX10.5.6/gcc4.0.1    0.29   * leescheme-5.9, 64 bit executable, xcode -O3, MacBookPro v2 2.16GHz
; Intel Core 2 Duo 1.83GHz/OSX10.4.11           0.3    * leescheme-5.9, 32 bit, GCC 4 -O3 -fomitframepointer, Mini 1.83
; Intel Core 2 Duo 1.83GHz/OSX10.5.1            0.3    * leescheme-5.9, 32 bit executable, xcode -O3, Mini 1.83
; AMD 1900/gcc3.2.2/chicken-1.0                 0.3    * -optimize-level 3 -block, 23k executable
; Intel Core 2 Duo 1.83GHz/OSX10.4.11           0.31   * leescheme-5.9, 64 bit, GCC 4 -O3 -fomitframepointer, Mini 1.83
; AMD 3000+/Win2k/vc2005                        0.34   * 32 bit executable, 32m heap
; AMD 2400/win2k/msvc5sp3                       0.38   ** normal optimized build, but with a 32m heap (it never gc's)
; Intel Core i7 2.80GHz/OSX10.6.2               0.40   * JScheme-0.1 (5 iteration average), 64 bit java 1.6.0_20
; AMD 3800x2/linux2.6.13/gcc3.4                 0.42   * 32 bit executable
; AMD 3000+/Win2k/vc2005                        0.42   * 32 bit executable
; AMD 3800x2/linux2.6.13/gcc3.4                 0.45   * 64 bit executable
; AMD 3800x2/linux2.6.19/gcc4.1.1               0.46   * 64 bit executable
; AMD 2400/win2k/msvc5sp3                       0.49   ** normal optimized build, 2m heap (it gc's 23 times)
; AMD 1138/RH73/chicken-0.1072                  0.5    * -optimize-level 3 -block, 28k executable
; AMD 1138/RH73/Bigloo2.5a  unoptimized         0.5    * 12k executable
; AMD Athlon 1900 XP, linux 2.4.20, gcc3.2.3    0.52   * leescheme-5.8, -O3 -fomitframepointer, 32M heap
; AMD Athlon 1900 XP, linux 2.4.20, gcc3.2.3    0.62   * leescheme-5.8, -O3 -fomitframepointer
; AMD Athlon 1900 XP, RedHat 8.0, gcc3.2        0.63   * leescheme-5.8
; Mac Mini 2GHz, Snow Leopard, Java 1.6.0       0.68   * jscheme-0.1 (5 iteration average)
; AMD Athlon 1800 XP, linux 2.4.20, gcc3.2.3    0.75   * leescheme-5.8, -O3 -fomitframepointer
; AMD Athlon 1800 XP @ 1145MHz (100MHz bus)     0.77   * leescheme-5.8, -O3 -fomitframepointer
; AMD TBird-1138, RedHat 7.3, gcc2.96           0.85   * leescheme-5.8
; iMac 800MHz, OS 10.2/gcc3.1                   0.9    * chicken 1.0, -optimize-level 3 -block
; Mac Mini G4-1.25GHx, 256M, gcc4.0             1.0    * leescheme-5.8, -Os, 32m heap (it never gc's)
; AMD TBird-1138, Windows2000, MSVC 5.0         1.0    * scheme5.7
; AMD TBird-1008, Windows2000, MSVC 5.0         1.17   * scheme5.7
; Powerbook G4, 1GHz, 512M, OSX10.4.8/GCC4.01   1.2    * leescheme-5.9 -Os
; AMD TBird-990, Windows2000, MSVC 5.0          1.2    * scheme5.7
; Intel P4-1000, RedHat 7.3, gcc2.96            1.2    * leescheme-5.8
; Mac Mini G4-1.25GHz, 256M, gcc4.0             1.0    * leescheme-5.8, -Os
; AMD TBird-900, RedHat 7.1, gcc2.96            1.3    * leescheme-5.8
; AMD TBird-900, Windows2000, MSVC 5.0          1.3    * scheme5.7
; AMD TBird-900, OpenBSD2.8, gcc2.95.3          1.4    * leescheme-5.8
; AMD TBird-1138, RedHat 7.3, gcc2.96           1.4    * leescheme-5.8 (totally dynamic, debug mode)
; iMac PPC-800, Mac OS 10.1.2, gcc 2.95.2       1.4    * leescheme-5.8
; iMac PPC-800, Mac OS 10.2.2, gcc 3.1          1.6    * leescheme-5.8
; AMD Athlon-500, Mandrake7.2/Linux2.4.1        1.8    * leescheme-5.8 (8m heap)
; AMD Athlon-500, Mandrake7.2/Linux2.4.1        2.1    * leescheme-5.8 (2m heap)
; AMD Athlon-500, Windows2000, MSVC 5.0         2.1    * scheme5.7
; AMD Athlon-500, Mandrake7.2/Linux2.4.1        2.2    * scheme5.7
; EPIA 600MHz Samuel 2, Gentoo, gcc3.3.5        2.3    * leescheme-5.8
; Sony PictureBook Crusoe-600, mdk7.2, gcc 2.95 2.3    * scheme5.7 (686=2.3, 586=2.4, 486=2.4)
; AMD TBird-950, OpenBSD2.8/VMWare, gcc2.95.3   2.5    * scm5d4
; AMD1138/RH73/chicken-0.1072 -optimize-level 3  2.7   * chicken -optimize-level 3 -heap-size 4m
; Celeron 533 gcc 2.95.2                        2.8    * scheme5.7
; AMD1138/RH73/chicken-0.1072                   3.2    * not optimized. See above for optimized compile
; Sony PictureBook Crusoe-600, WinMe, MSVC5     3.2    * scheme5.7
; AMD Athlon-500, Mandrake7.2/Linux2.4.1        3.3    * scm5d4
; Dell Dimension XPS H266 Pentium II, NT4.0     3.8    * scheme5.7
; IBM Thinkpad 770Z, Win2k, MSVC5               3.8    * scheme5.7
; Shuttle 604, AMD K6-266 at 266, Mandrake 6.1  3.9    * scheme5.7
; AMD1138/RH73/Chicken0.1072 interpreter        4.8    * can be compiled to go fast (see above!)

; noname Pentium Pro 200, MSVC 4, NT4.0         5.3
; Micron P166, MSVC4.0, NT 4.0                  5.8
; HP Pavilion 166, Win95, MSVC 5.0              6.0    * scheme5.7
; Micron P166, MSVC4.0, Win95                   6.2
; Gateway P150 MMX, MSVC 4.0, Win95             6.4
; Sun UltraSparc 166, GCC, Solaris              8.1
; Dell Pentium 100, MSVC 4.0, NT 4.0           10.0    * this is the reference machine.
; AMD Athlon-500, Mandrake7.2/Linux2.4.1       10.0    * gambit C, 3.0 - gsi
; Sun HyperSparc 50, GCC, Solaris              13.3
; Toshiba 40MHz 486DX2, Linux 2.0, GCC2.7.2)   37.9    * minimum acceptable performance.
;
; *** Java implementation:
; NOTE: for JDK, assume -ms32m -mx32m
; AMD TBird-1139, RH73, jdk1.4, Kawa1.6.99      1.9    !!!but no full continuations, numerics
; AMD Athlon-500, Windows2000, jview 4.0        8.2
; AMD Athlon-1138, Redhat7.3, jdk1.4.0          8.2
; SISC, on same Athlon-1138/RH7.3/jdk1.4sisc   10.5
; AMD Athlon-500, Windows2000, jdk1.2.2_05     17.3
; Dell XPS H266, NT4.0, jview 2.02             23.4
; Dell XPS H266, NT4.0, jdk1.1.6 (symJIT)      28.1
; Dell XPS H266, NT4, Kawa 1.6                 43.0
; AMD K6-333, Redhat 5.0, jdk1.1.6/TYA1.0      52.2
; AMD K6-333, Redhat 5.0, Kawa1.6              59.0
; noname Pentium Pro 200, jview1.0            107.7
; Gateway P150 MMX, jscheme0.1, Win95         114.0
; AMD K6-333, Redhat 5.0, jdk1.1.6 (no JIT)   113.2
; Micron P166, jscheme0.1, Win95              150.0
; Dell P100, jscheme0.1, NT 4.0               250.0
;
;**** Old numbers:
;
; Gw2k Pentium 150, MSVC 4.0                    
;	MSVC, bind primops                      1.8    *
; Dell Pentium 100, MSVC 4.0                    3.0
;	MSVC, bind primops                      2.3    *
;	MSVC, no type check                     2.1
;	GCC, Linux                              2.9
;	GCC, Linux, bind primops                2.2    *
;	GCC, no type check (deschutes)          2.0
; HP 99MHz PA_RISC (hpux cc)                    4.2
; Mac 8100 PPC 601 100MHz, CW7			4.7
;	CW7, bind primops                       3.4    *
;	CW7, no type check                      3.2
;	CW7, 68k code, emulated: doesn't run
; Sparcstation 10 (sunos4.x/gcc)                5.5
;	scm4c5                                  7.8    !
;	scheme53,fully dynamic                  6.2
;	scheme53,bind primops                   5.0    *
;	scheme53,fast and loose                 4.5
; Compaq DeskPro 66M (nextstep)                 6.2
; Intel GX/Pro 66DX2 (nextstep)                 6.4
; Dell 66/M (nextstep)                          6.5
; IBM ValuePoint 66DX2/D (nextstep)             6.9
; Dell ME/DGX 50DX2 (nextstep)                  8.6
; Toshiba 1950 (40MHz 486DX2 laptop)           10.8	(scheme53, SCC under DOS)
;   scheme55, Linux/GCC,                       13.7
;   scheme55, Linux/GCC, bind primops           8.5	*
;   scheme55, Linux/GCC, no type check          7.7
;	scheme53, fully dynamic                13.2
;	scheme53, bind-primops                  8.8
;	scheme53,no type check (inline car)	8.1
;	scm4c5                                   29    !
; NextStation Turbo (33MHz 68040)              12.5     (in new numbers: 42)
; NextStation                                  17.2
; Next Cube 030                                74.4     (in new numbers: 247)
; Mac Plus                                    560.0     (in new numbers: 1866)
;

(define (provide . args) #t)

(load "sort.scm")

;;;; "pi.scm", program for computing digits of numerical value of PI.
;;; Copyright (C) 1991 Aubrey Jaffer.
;;; See the file `COPYING' for terms applying to this program.
;;; (pi <n> <d>) prints out <n> digits of pi in groups of <d> digits.
;;; 'Spigot' algorithm origionally due to Stanly Rabinowitz.
;;; This algorithm takes time proportional to the square of <n>/<d>.
;;; This fact can make comparisons of computational speed between systems
;;; of vastly differring performances quicker and more accurate.
;;; Try (pi 100 5)
;;; The digit size <d> will have to be reduced for larger <n> or an
;;; overflow error will occur.
(define (pi n d)
  (set! n (+ (quotient n d) 1))
  (let ((m (quotient (* n d 3322) 1000)))
     (let ((r (do ((s 1 (* 10 s))
		 (i 0 (+ 1 i)))
		((>= i d) s)))
	   (a (make-vector (+ 1 m) 2)))
	(vector-set! a m 4)
	(do ((j 1 (+ 1 j))
		(q 0 0)
		(b 2 (remainder q r)))
	    ((> j n))
	    (do ((k m (- k 1))) ((zero? k))
		(set! q (+ q (* (vector-ref a k) r)))
		(let ((t (+ 1 (* 2 k))))
		    (vector-set! a k (remainder q t))
		    (set! q (* k (quotient q t)))))
	    (let ((s (number->string (+ b (quotient q r)))))
	    (do ((l (string-length s) (+ 1 l)))
		((>= l d) (display s))
		(display #\0)))
	    (display (if (zero? (modulo j 10)) #\newline #\ )))
	(newline))))

;;
;; End of Jaffer's nagware
;;

(define foo (do ((i 0 (+ i 1)) (l '() (cons i l))) ((>= i 30000) l)))
(define (benchmark)      (sort foo <)
    (sort foo >)
    (pi 100 5)
    (pi 518 4))

(let ((t0 (system:timestamp)))
	(benchmark)
	(print (/ (- (system:timestamp) t0) 1000.0) " seconds")) 



;;EOF
