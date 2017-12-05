/*
 * Memoized Fibonacci
 */

type memo = Hashtbl.t(int, int);

let rec f = (~n: int, ~memo: memo): int => {
  let result =
    switch n {
    | x when x <= 0 => 0
    | x when x <= 2 => 1
    | x =>
      switch (Hashtbl.find(memo, n)) {
      | y => y
      | exception Not_found =>
        let prev = f(~n=(x - 1), ~memo);
        let secondPrev = f(~n=(x - 2), ~memo);
        prev + secondPrev;
      };
    };

  Hashtbl.add(memo, n, result);

  result;
};

let fibonacci = (n: int): int => {
  let memo = Hashtbl.create(~random=true, n);
  f(~n, ~memo);
};
