  $ ./test006.exe
  fun q -> substo (v varX) varX (v varY) q, 1 answer {
  q=V ("y");
  }
  fun q -> evalo (abs varX (v varX)) q, 1 answer {
  q=Abs ("x", V ("x"));
  }
  fun q -> evalo (abs varX (v varX)) q, 2 answers {
  q=Abs ("x", V ("x"));
  }
  fun q -> evalo (app (abs varX (v varX)) (v varY)) q, 1 answer {
  q=V ("y");
  }
  fun q -> evalo (app (abs varX (v varX)) q) (v varY), 1 answer {
  q=V ("y");
  }
  fun q -> evalo (app (abs varX q) (v varY)) (v varY), 1 answer {
  q=V ("x");
  }
  fun q -> evalo (app (v varX) (v varX)) q, 1 answer {
  q=App (V ("x"), V ("x"));
  }
  fun q -> evalo (v varX) q, 1 answer {
  q=V ("x");
  }
  fun q -> evalo (app q (v varX)) (v varX), 1 answer {
  q=Abs (_.20, V (_.20));
  }
  fun q r -> evalo (app r q) (v varX), 1 answer {
  q=V ("x"); r=Abs (_.20, V (_.20));
  }
  fun q r s -> a_la_quine q r s, 2 answers {
  q=Abs (_.21, V (_.21)); r=Abs (_.21, V (_.21)); s=Abs (_.21, V (_.21));
  q=Abs (_.21, V (_.21)); r=Abs (_.21, Abs (_.21, V (_.21))); s=Abs (_.21, Abs (_.21, V (_.21)));
  }
