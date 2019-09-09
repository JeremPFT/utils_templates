with Gnat.Traceback.Symbolic;
with Gnat.Traceback;
--  with GNAT.Exception_Traces;
with System.Assertions;

with Ada.Exceptions;
with Ada.Text_IO;

--  with GNAT.Traceback;
--  with GNAT.Traceback.Symbolic;

with Aunit.Run;
with Aunit.Reporter.Text;

with Templates.Test.Suites;

procedure Templates.Test.Run is

  pragma Check_Policy (Debug, On); -- -gnata

  package T_IO renames Ada.Text_IO;

  pragma Debug ( T_IO.Put_Line ("templates.test.run") );

  procedure Call_Stack is
    Trace  : Gnat.Traceback.Tracebacks_Array (1..1_000);
    Length : Natural;
  begin
    Gnat.Traceback.Call_Chain (Trace, Length);
    T_IO.Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback (Trace (1..Length)));
  end Call_Stack;

  Reporter : Aunit.Reporter.Text.Text_Reporter;

  use Ada.Exceptions;

begin
  declare
    procedure Run is new Aunit.Run.Test_Runner (Templates.Test.Suites.Suite);
  begin
    Run (Reporter);
  end;

exception
  when Error: System.Assertions.Assert_Failure =>
    Call_Stack;
    T_IO.Put ("ASSERT exception: ");
    T_IO.Put_Line (Exception_Information(Error));
    raise;

  when Error: others =>
    Call_Stack;
    T_IO.Put ("Unexpected exception: ");
    T_IO.Put_Line (Exception_Information(Error));
    raise;

    --  GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

end Templates.Test.Run;
