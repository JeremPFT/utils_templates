--  with ada files

--  with others libraries
with Templates_Parser;

--  with project files
--  with Model.Element;

package Templates is

  --  Template_Directory : constant String :=
  --  "d:/Users/jpiffret/AppData/Roaming/Dropbox/projets perso/ada/code_generator/templates_files";
  --  "/home/jeremy/workspace/ada/code_generator/templates_files/";

  -----------------------------------------------------------------------------
  --  types
  -----------------------------------------------------------------------------

  type Tag_Names_Array_T is
    array (Positive range <>)
    of access constant String;

  Tag_Not_Found : exception;

  No_Tag : constant Tag_Names_Array_T (1 .. 0) := (others => null);

  function "+" (Item : String) return not null access String
    is (new String'(Item));

  type Object_T is tagged private;

  type Reference_T is access all Object_T;

  type Class_T is access all Object_T'Class;

  -----------------------------------------------------------------------------
  --  create, clean, initialize
  -----------------------------------------------------------------------------

  function Create
    (Template_Directory_Name : in String;
     Template_File_Name      : in String;
     Tag_Names               : in Tag_Names_Array_T := No_Tag)
    return not null access Object_T
  with
    Pre => Template_Directory_Name /= "" and then Template_File_Name /= "";

  procedure Free (Object : in out Reference_T);

  -----------------------------------------------------------------------------
  --  queries
  -----------------------------------------------------------------------------

  function Get_Template_Directory_Name
    (Self : in Object_T)
    return String;

  -----------------------------------------------------------------------------

  function Get_Template_File_Name
    (Self : in Object_T)
    return String;

  -----------------------------------------------------------------------------

  function Has_Tag
    (Self     : in Object_T;
     Tag_Name : in String)
    return Boolean
  with Pre => Tag_Name /= "";

  -----------------------------------------------------------------------------

  function Get_Value
    (Self     : in Object_T;
     Tag_Name : in String;
     Index    : in Positive)
    return String
  with Pre => Self.Has_Tag (Tag_Name);

  -----------------------------------------------------------------------------

  function Number_Of_Elements_In_Tag
    (Self     : in Object_T;
     Tag_Name : in String)
    return Natural;

  -----------------------------------------------------------------------------

  function File_Exists
    (Self : in Object_T'Class)
    return Boolean;

  -----------------------------------------------------------------------------

  function To_String
    (Self : in Object_T)
    return String
    with Pre => File_Exists (Self);

  -----------------------------------------------------------------------------
  --  commands
  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     String)
  with Pre => Self.Has_Tag (Tag_Name);

  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     Integer)
  with Pre => Self.Has_Tag (Tag_Name);

  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     Boolean)
  with Pre => Self.Has_Tag (Tag_Name);

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------

  package Tp renames Templates_Parser;

  type Tag_Array_T is array (Positive range <>) of Tp.Tag;

  type Object_T is tagged record
    Template_Directory_Name : access String             := null;
    Template_File_Name      : access String             := null;
    Tag_Names               : access Tag_Names_Array_T  := null;
    Tags                    : access Tag_Array_T        := null;
    Translate_Table         : access Tp.Translate_Table := null;
  end record;

  -----------------------------------------------------------------------------

end Templates;
