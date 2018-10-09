module HelloWorldTests
// test(s) so simple, only Expecto or the test hook-up could cause failure
open Expecto

[<Tests>]
let myFirstTests =
    testList "myFirstTests" [
        testCase "MyFirstTest" <|
            fun () -> Expect.isTrue(1 = 1) "1 doesn't equal 1?"
        testList "inlineData conversion" <|
            List.ofSeq (testParam [3;5] [
                    "inlineData",
                        fun (values:int list) () ->
                            Expect.all values (fun v -> v % 2 = 1) "odd wasn't odd"
                ])
    ]

