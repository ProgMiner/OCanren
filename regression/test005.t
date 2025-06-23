  $ ./test005.exe
  fun q -> lookupo varX (inj_list_p []) q, 1 answer {
  }
  fun q -> lookupo varX (inj_list_p [varX, v varX]) q, 1 answer {
  q=V ("x");
  }
  fun q -> lookupo varX (inj_list_p [varY, v varY; varX, v varX]) q, 1 answer {
  q=V ("x");
  }
  fun q -> lookupo q (inj_list_p [varY, v varY; varX, v varX]) (v varX), 1 answer {
  q="x";
  }
  fun q -> lookupo q (inj_list_p [varY, v varY; varX, v varX]) (v varY), 1 answer {
  q="y";
  }
  fun q -> lookupo varX q (v varY), 1 answer {
  q=[("x", V ("y")) | _.13];
  }
  fun q -> infero (abs varX (v varX)) q, 1 answer {
  q=Arr (_.22, _.22);
  }
  fun q -> infero (abs varF (abs varX (app (v varF) (v varX)))) q, 1 answer {
  q=Arr (Arr (_.43, _.32), Arr (_.43, _.32));
  }
  fun q -> infero (abs varX (abs varF (app (v varF) (v varX)))) q, 1 answer {
  q=Arr (_.74, Arr (Arr (_.74, _.32), _.32));
  }
  fun q -> infero (abs varX (app (v varX) (v varX))) q, 1 answer {
  q=Arr (mu 33 <Arr (_.33, _.18)>, _.18);
  }
  fun q -> infero q (arr (p varX) (p varX)), 1 answer {
  q=Abs (_.32, V (_.32));
  }
