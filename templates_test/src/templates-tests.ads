with AUnit;
with AUnit.Test_Cases;

package Templates.Tests
is

  type Test_Case is new AUnit.Test_Cases.Test_Case with private;

  overriding
  function Name
    (Test : Test_Case)
    return AUnit.Message_String
    is ( AUnit.Format ( "Templates" ) ) ;

  overriding
  procedure Register_Tests
    ( T: in out Test_Case );

  not overriding
  procedure Setup_Tests
    ( T : in out Test_Case );

  overriding
  procedure Tear_Down (T : in out Test_Case);

  -----------------------------------------------------------------------------
  --  tests cases
  -----------------------------------------------------------------------------

  procedure Test_Creation
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Add_Tag_Value_Number_Incremented
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Add_Tag_Value_Read_Value
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Add_Tag_Value_Result
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Bad_File_Name_Then_Exception
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Tag_Name_Not_Used_Then_Exception
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

  procedure Test_Tag_Name_Unknown_Then_Exception
    ( T : in out AUnit.Test_Cases.Test_Case'Class );

private

  type Test_Case is new AUnit.Test_Cases.Test_Case with record
    Template : access Templates.Object_T'Class;
  end record;

end Templates.Tests;
