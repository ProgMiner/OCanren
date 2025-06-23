  $ ./test001.exe
  fun q -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4]), 1 answer {
  q=[1; 2];
  }
  fun q -> reverso q (ilist [1; 2; 3; 4]), 1 answer {
  q=[4; 3; 2; 1];
  }
  fun q -> reverso (ilist [1; 2; 3; 4]) q, 1 answer {
  q=[4; 3; 2; 1];
  }
  fun q -> reverso q (ilist [1]), 2 answers {
  q=[1];
  }
  fun q -> reverso (ilist [1]) q, 1 answer {
  q=[1];
  }
  fun q -> a_and_b q, 1 answer {
  q=7;
  }
  fun q -> a_and_b' q, 2 answers {
  q=6;
  q=5;
  }
  fun q -> fives q, 10 answers {
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  }
  fun q -> success, 1 answer {
  q=_.10;
  }
  fun q -> OCanren.Fresh.one (fun n -> delay (fun () -> q === n % Std.nil ())), 1 answer {
  q=[_.11];
  }
  fun q -> OCanren.Fresh.three (fun a b c -> delay (fun () -> q === a % b % c)), 1 answer {
  q=[[_.11 | _.12] | _.13];
  }
  fun q -> reverso (ilist []) (ilist []), 1 answer {
  q=_.10;
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.11];
  }
  fun q r -> appendo q (ilist []) r, 4 answers {
  q=[]; r=[];
  q=[_.12]; r=[_.12];
  q=[_.12; _.15]; r=[_.12; _.15];
  q=[_.12; _.15; _.18]; r=[_.12; _.15; _.18];
  }
  fun q -> reverso q q, 1 answer {
  q=[];
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.11];
  }
  fun q -> reverso q q, 3 answers {
  q=[];
  q=[_.11];
  q=[_.14; _.14];
  }
  fun q -> reverso q q, 10 answers {
  q=[];
  q=[_.11];
  q=[_.14; _.14];
  q=[_.14; _.34; _.14];
  q=[_.14; _.50; _.50; _.14];
  q=[_.14; _.75; _.113; _.75; _.14];
  q=[_.14; _.106; _.156; _.156; _.106; _.14];
  q=[_.14; _.149; _.211; _.229; _.211; _.149; _.14];
  q=[_.14; _.192; _.272; _.296; _.296; _.272; _.192; _.14];
  q=[_.14; _.259; _.357; _.381; _.402; _.381; _.357; _.259; _.14];
  }
  fun q r -> two_vars q r, 1 answer {
  q=_.11; r=_.11;
  }
  fun q -> occurs q, all answers {
  q={_.10 = [1 | _.10]};
  }
