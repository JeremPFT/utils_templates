with Ada.Text_IO;
with Gnat.Source_Info;
with Ada.IO_Exceptions;

with Assertions;
with Templates.Test.Expected;

package body Templates.Tests
is

  -----------------------------------------------------------------------------
  --  debug configuration
  -----------------------------------------------------------------------------

  pragma Check_Policy (Debug, On); --  -gnata

  -----------------------------------------------------------------------------
  --  tests preparations
  -----------------------------------------------------------------------------

  package T_IO renames Ada.Text_IO;
  package Expected renames Templates.Test.Expected;

  pragma Debug ( T_IO.Put_Line (Gnat.Source_Info.File & "::" &
                                  Gnat.Source_Info.Enclosing_Entity) );
  pragma Debug ( T_IO.Put_Line (Gnat.Source_Info.Compilation_Iso_Date & "::" &
                                  Gnat.Source_Info.Compilation_Time) );

  procedure Setup_Tests
    ( T : in out Test_Case )
  is
  begin
    pragma Debug ( T_IO.Put_Line (Gnat.Source_Info.File & "::" &
                                    Gnat.Source_Info.Enclosing_Entity) );
    pragma Debug ( T_IO.Put_Line (Gnat.Source_Info.Compilation_Iso_Date & "::" &
                                    Gnat.Source_Info.Compilation_Time) );
    T.Template := Expected.Template_1;
  end Setup_Tests;

  -- registering tests

  procedure Register_Tests
    ( T: in out Test_Case )
  is
    use Aunit.Test_Cases.Registration;
  begin
    Register_Routine
      (T,
       Test_Creation'Access,
       "Test_Creation");
    Register_Routine
      (T,
       Test_Add_Tag_Value_Number_Incremented'Access,
       "Test_Add_Tag_Value_Number_Incremented");
    Register_Routine
      (T,
       Test_Add_Tag_Value_Read_Value'Access,
       "Test_Add_Tag_Value_Read_Value");
    Register_Routine
      (T,
       Test_Add_Tag_Value_Result'Access,
       "Test_Add_Tag_Value_Result");
    Register_Routine
      (T,
       Test_Bad_File_Name_Then_Exception'Access,
       "Test_Bad_File_Name_Then_Exception");
    Register_Routine
      (T,
       Test_Tag_Name_Not_Used_Then_Exception'Access,
       "Test_Tag_Name_Not_Used_Then_Exception");
    Register_Routine
      (T,
       Test_Tag_Name_Unknown_Then_Exception'Access,
       "Test_Tag_Name_Unknown_Then_Exception");
  end Register_Tests;

  procedure Tear_Down
    (T : in out Test_Case)
  is
    procedure Print_And_Clean
      (Template : in out Templates.Class_T)
    is
    begin
      if Template /= null then
        T_IO.Put_Line ("cleaning template " & Template.Get_Template_File_Name);

        Templates.Free (Templates.Reference_T (Template));
      end if;
    end Print_And_Clean;

  begin
    Print_And_Clean (T.Template);
  end Tear_Down;

  -----------------------------------------------------------------------------
  -- tests definitions
  -----------------------------------------------------------------------------

  procedure Test_Creation
    ( T : in out Aunit.Test_Cases.Test_Case'Class )
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
  begin
    Setup_Tests (Test);

    Assertions.Check_String (Got      => Test.Template.Get_Template_Directory_Name,
                             Expected => Expected.Directory);

    Assertions.Check_String (Got      => Test.Template.Get_Template_File_Name,
                             Expected => Expected.Template_1_Filename);

    declare
      Value : Integer;
    begin
      Value := Test.Template.Number_Of_Elements_In_Tag ( Expected.Template_1_Tag_1 );
      Assertions.Check_Integer (Got => Value, Expected => 0);

      Value := Test.Template.Number_Of_Elements_In_Tag ( Expected.Template_1_Tag_2 );
      Assertions.Check_Integer (Got => Value, Expected => 0);
    end;

  end Test_Creation;

  -----------------------------------------------------------------------------

  procedure Test_Add_Tag_Value_Number_Incremented
    ( T : in out Aunit.Test_Cases.Test_Case'Class )
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
  begin
    Setup_Tests (Test);

    Test.Template.Add_Value (Tag_Name => Expected.Template_1_Tag_1,
                             Value    => Expected.Template_1_Tag_1_Value_1);

    declare
      Value : constant Integer :=
        Test.Template.Number_Of_Elements_In_Tag
          ( Tag_Name => Expected.Template_1_Tag_1 );
    begin
      Assertions.Check_Integer (Got => Value, Expected => 1);
    end;

  end Test_Add_Tag_Value_Number_Incremented;

  -----------------------------------------------------------------------------

  procedure Test_Add_Tag_Value_Read_Value
    ( T : in out Aunit.Test_Cases.Test_Case'Class )
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
  begin
    Setup_Tests (Test);

    Test.Template.Add_Value (Tag_Name => Expected.Template_1_Tag_1,
                             Value    => Expected.Template_1_Tag_1_Value_1);

    declare
      Value : constant String :=
        Test.Template.Get_Value (Tag_Name => Expected.Template_1_Tag_1,
                                 Index    => 1);
    begin
      Assertions.Check_String (Got      => Value,
                               Expected => Expected.Template_1_Tag_1_Value_1_Img);
    end;

  end Test_Add_Tag_Value_Read_Value;

  -----------------------------------------------------------------------------

  procedure Test_Add_Tag_Value_Result
    (T : in out Aunit.Test_Cases.Test_Case'Class)
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
  begin
    Setup_Tests (Test);

    Test.Template.Add_Value (Tag_Name => Expected.Template_1_Tag_1,
                             Value    => Expected.Template_1_Tag_1_Value_1);

    declare
      Value : constant String :=
        Test.Template.To_String;
    begin
      Assertions.Check_String (Got      => Value,
                               Expected => "@_x_@" & Ascii.Lf & "1" & Ascii.Lf);
    end;

  end Test_Add_Tag_Value_Result;

  -----------------------------------------------------------------------------

  procedure Test_Bad_File_Name_Then_Exception
    (T : in out Aunit.Test_Cases.Test_Case'Class)
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
    Is_Valid : Boolean := False;
  begin
    declare
    begin
      Test.Template := Create (Template_Directory_Name => Expected.Directory,
                               Template_File_Name      => "inexistant_file");
      T_IO.Put_Line (Test.Template.To_String);
    exception
      when E: ADA.IO_Exceptions.Name_Error =>
        Is_Valid := True;
        pragma Unreferenced (E);
    end;

    Assertions.Check_Boolean (Got      => Is_Valid,
                              Expected => True);
  end Test_Bad_File_Name_Then_Exception;

  -----------------------------------------------------------------------------

  procedure Test_Tag_Name_Not_Used_Then_Exception
    (T : in out Aunit.Test_Cases.Test_Case'Class)
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
    pragma Unreferenced (Test);
  begin
    Assertions.Not_Implemented;
  end Test_Tag_Name_Not_Used_Then_Exception;

  -----------------------------------------------------------------------------

  procedure Test_Tag_Name_Unknown_Then_Exception
    (T : in out Aunit.Test_Cases.Test_Case'Class)
  is
    Test : Test_Case'Class renames Test_Case'Class (T);
    pragma Unreferenced (Test);
  begin
    Assertions.Not_Implemented;
  end Test_Tag_Name_Unknown_Then_Exception;

end Templates.Tests;
