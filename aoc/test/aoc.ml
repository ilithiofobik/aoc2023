open Aoc

let () =
  Alcotest.(
    run
      "Aoc"
      [ ( "Day1"
        , [ test_case "result1" `Quick (fun () -> assert (Day1.result1 = 55488))
          ; test_case "result2" `Quick (fun () -> assert (Day1.result2 = 55614))
          ] )
      ; ( "Day2"
        , [ test_case "result1" `Quick (fun () -> assert (Day2.result1 = 2149))
          ; test_case "result2" `Quick (fun () -> assert (Day2.result2 = 71274))
          ] )
      ; ( "Day3"
        , [ test_case "result1" `Quick (fun () -> assert (Day3.result1 = 544664))
          ; test_case "result2" `Quick (fun () -> assert (Day3.result2 = 84495585))
          ] )
      ; ( "Day4"
        , [ test_case "result1" `Quick (fun () -> assert (Day4.result1 = 25004))
          ; test_case "result2" `Quick (fun () -> assert (Day4.result2 = 14427616))
          ] )
      ])
;;
