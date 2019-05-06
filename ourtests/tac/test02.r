a : int;
pa : °int := §a;
ppa : °°int := §pa;

array : Array <int> := [1,2,3];

proc main () is
 f();

 parray : °Array <int> := §array;
 x : int := array[1];
 y : int := (°parray)[++a];

 proc f () is
   y : °°int := §°ppa;
   x : int := °°ppa;
   °°ppa := x;
 end f

 a : real;
 a := °pa;
end main

proc f () is
  array : Array <Array <real>> := [[1,2,3],
                                  array,
                                  create Array <real> (3)];
  x : real := array[1][3];

  return;

  x := f(array[1]);

  func f (x : ref Array<real>(3)) -> (r : real) is
    r := 2 + 20*2;
    const n : int := 7;
    return;
    r := array[1][n % 2];
  end f
end f


========================================================
== EXPECTED TAC:

{=            a@1,1             =int   0
            pa@2,1            =addr  &a@1,1
            ppa@3,1           =addr  &pa@2,1
            array@5,1[0]      =int   1
            array@5,1[32]     =int   2
            array@5,1[64]     =int   3
            call main@7,6
            halt 
 
 main@7,6:  //Preamble main
            //Body main
            call f@14,7
 l1:        parray@10,2       =addr  &array@5,1
 l2:        x@11,2            =int   array@5,1[32]
 l3:        a@1,1             =int   a@1,1 +int 1
            t0                =int   a@1,1 *int 32
            t1                =addr  *parray@10,2
            y@12,2            =int   t1[t0]
 l4:
 l5:        a@20,2            =real  0.0
 l9:        t4                =real  conv_int_to_real *pa@2,1
            a@20,2            =real  t4
 l0:        //Postamble main
            return 
 
 f@14,7:    //Preamble f
            //Body f
            y@15,4            =addr  ppa@3,1
 l7:        t2                =addr  *ppa@3,1
            x@16,4            =int   *t2
 l8:        t3                =addr  *ppa@3,1
            *t3               =int   x@16,4
 l6:        //Postamble f
            return 
 
 f@24,6:    //Preamble f
            //Body f
            t5                =real  conv_int_to_real 1
            array@25,3[0]     =real  t5
            t6                =real  conv_int_to_real 2
            array@25,3[64]    =real  t6
            t7                =real  conv_int_to_real 3
            array@25,3[128]   =real  t7
            t8                =real  conv_int_to_real array@5,1[0]
            array@25,3[192]   =real  t8
            t9                =real  conv_int_to_real array@5,1[32]
            array@25,3[256]   =real  t9
            t10               =real  conv_int_to_real array@5,1[64]
            array@25,3[320]   =real  t10
            array@25,3[384]   =real  0.0
            array@25,3[448]   =real  0.0
            array@25,3[512]   =real  0.0
 l11:       x@28,3            =real  array@25,3[384]
 l12:       goto l10
 l13:       param array@25,3[192]
            t11               =      call f@34,8
            x@28,3            =real  t11
 l14:
 l10:       //Postamble f
            return 
 
 f@34,8:    //Preamble f
            //Body f
            t12               =real  conv_int_to_real 42
            r@34,39           =real  t12
 l16:
 l17:       goto l15
 l18:       r@34,39           =real  array@25,3[256]
 l15:       //Postamble f
            return r@34,39
=}