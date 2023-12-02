open Aoc

let () =
  let open Alcotest in
  run "Aoc"
    [
      ( "Day1",
        [
          test_case "result1" `Slow (fun () -> assert (Day1.result1 = 55488));
          test_case "result2" `Slow (fun () -> assert (Day1.result2 = 55614));
        ] );
      ( "Day2",
        [
          test_case "result1" `Slow (fun () -> assert (Day2.result1 = 2149));
          test_case "result2" `Slow (fun () -> assert (Day2.result2 = 71274));
        ] );
    ]
