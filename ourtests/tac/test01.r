const n : int := 3;

proc main () is
 fun();
end main

proc fun () is
 == could be optimized assigning call to dst.
 dst : Array<int>(4) := gVal([1,2,3,4]);
 arrarr : Array < Array <int> > := gValRes(dst);
 dst := arrarr[n-3];
 parr : °Array <int> (4) := gRef(dst);
 gRes(°parr);
end fun

func gVal (x : Array<int>(4)) -> (r : Array<int>(4)) is
r := x;
x := create Array<int> (4);
end gVal

func gValRes (x : valres Array<int>(4)) -> (r : Array<Array<int>(4)>(2)) is
r := [x,x];
end gValRes

func gRef (x : ref Array<int>(4)) -> (r : °Array<int>(4)) is
r := §x;
end gRef

proc gRes (x : res Array<int>(4)) is
 x := [n, n+1, -((n+10) % 2), n*(-2)];
 x[1] ++;
 y : int := x[--x[1]++];
 p : °int := §x[x[1]];
 x[n] := °p;
end gRes


================================================================
== EXPECTED TAC:

{=                call main@3,6
                halt 
 
 main@3,6:      //Preamble main
                //Body main
                call fun@7,6
 l0:            //Postamble main
                return 
 
 fun@7,6:       //Preamble fun
                //Body fun
                t0[0]              =int   1
                t0[32]             =int   2
                t0[64]             =int   3
                t0[96]             =int   4
                param t0
                t1                 =      call gVal@16,6
                dst@9,2[0]         =int   t1[0]
                dst@9,2[32]        =int   t1[32]
                dst@9,2[64]        =int   t1[64]
                dst@9,2[96]        =int   t1[96]
 l2:            param dst@9,2
                t2                 =      call gValRes@21,6
                arrarr@10,2[0]     =int   t2[0]
                arrarr@10,2[32]    =int   t2[32]
                arrarr@10,2[64]    =int   t2[64]
                arrarr@10,2[96]    =int   t2[96]
                arrarr@10,2[128]   =int   t2[128]
                arrarr@10,2[160]   =int   t2[160]
                arrarr@10,2[192]   =int   t2[192]
                arrarr@10,2[224]   =int   t2[224]
 l3:            dst@9,2[0]         =int   arrarr@10,2[0]
                dst@9,2[32]        =int   arrarr@10,2[32]
                dst@9,2[64]        =int   arrarr@10,2[64]
                dst@9,2[96]        =int   arrarr@10,2[96]
 l4:            param dst@9,2
                t3                 =      call gRef@25,6
                parr@12,2          =addr  t3
 l5:            param *parr@12,2
                call gRes@29,6
 l1:            //Postamble fun
                return 
 
 gVal@16,6:     //Preamble gVal
                //Body gVal
                r@16,35[0]         =int   x@16,12[0]
                r@16,35[32]        =int   x@16,12[32]
                r@16,35[64]        =int   x@16,12[64]
                r@16,35[96]        =int   x@16,12[96]
 l7:            x@16,12[0]         =int   0
                x@16,12[32]        =int   0
                x@16,12[64]        =int   0
                x@16,12[96]        =int   0
 l6:            //Postamble gVal
                return r@16,35
 
 gValRes@21,6:  //Preamble gValRes
                x@21,15_copy[0]    =int   x@21,15[0]
                x@21,15_copy[32]   =int   x@21,15[32]
                x@21,15_copy[64]   =int   x@21,15[64]
                x@21,15_copy[96]   =int   x@21,15[96]
                //Body gValRes
                r@21,45[0]         =int   x@21,15_copy[0]
                r@21,45[32]        =int   x@21,15_copy[32]
                r@21,45[64]        =int   x@21,15_copy[64]
                r@21,45[96]        =int   x@21,15_copy[96]
                r@21,45[128]       =int   x@21,15_copy[0]
                r@21,45[160]       =int   x@21,15_copy[32]
                r@21,45[192]       =int   x@21,15_copy[64]
                r@21,45[224]       =int   x@21,15_copy[96]
 l8:            //Postamble gValRes
                x@21,15[0]         =int   x@21,15_copy[0]
                x@21,15[32]        =int   x@21,15_copy[32]
                x@21,15[64]        =int   x@21,15_copy[64]
                x@21,15[96]        =int   x@21,15_copy[96]
                return r@21,45
 
 gRef@25,6:     //Preamble gRef
                //Body gRef
                r@25,39            =addr  &x@25,12
 l9:            //Postamble gRef
                return r@25,39
 
 gRes@29,6:     //Preamble gRes
                //Body gRes
                x@29,12_copy[0]    =int   3
                x@29,12_copy[32]   =int   4
                x@29,12_copy[64]   =int   -1
                x@29,12_copy[96]   =int   -6
 l11:           t4                 =int   x@29,12_copy[32]
                x@29,12_copy[32]   =int   t4 +int 1
 l12:           x@29,12_copy[32]   =int   x@29,12_copy[32] -int 1
                t5                 =int   x@29,12_copy[32]
                x@29,12_copy[32]   =int   t5 +int 1
                t6                 =int   t5 *int 32
                y@32,2             =int   x@29,12_copy[t6]
 l13:           t7                 =int   x@29,12_copy[32]
                t8                 =int   t7 *int 32
                t9                 =int   x@29,12_copy +int t8
                p@33,2             =addr  t9
 l14:           x@29,12_copy[96]   =int   *p@33,2
 l10:           //Postamble gRes
                x@29,12[0]         =int   x@29,12_copy[0]
                x@29,12[32]        =int   x@29,12_copy[32]
                x@29,12[64]        =int   x@29,12_copy[64]
                x@29,12[96]        =int   x@29,12_copy[96]
                return 
=}