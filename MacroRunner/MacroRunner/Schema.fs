module MacroRunner.Schema

type ISideEffect<'T> = 
    abstract Name:string
    abstract Description: string
    abstract Execution: 'T -> unit
type ISideEffect = 
    inherit ISideEffect<unit>




