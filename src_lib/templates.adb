--  with ada files
with Ada.Unchecked_Deallocation;
with Ada.Directories;

--  with others libraries

--  with project files
with File_IO;

package body Templates is

  -----------------------------------------------------------------------------

  function Tag_Position
    (Self     : in Object_T;
     Tag_Name : in String)
    return Positive;

  -----------------------------------------------------------------------------

  function Create
    (Template_Directory_Name : in String;
     Template_File_Name      : in String;
     Tag_Names               : in Tag_Names_Array_T := No_Tag)
    return not null access Object_T
  is
    Result : constant access Object_T :=
      new Object_T'(Template_Directory_Name => new String'(Template_Directory_Name),
                    Template_File_Name      => new String'(Template_File_Name),
                    Tag_Names               => new Tag_Names_Array_T'(Tag_Names),
                    Tags                    => <>,
                    Translate_Table         => <>);

    Tag_Number : constant Integer := Tag_Names'Length;

    Tags : Tag_Array_T (1 .. Tag_Number);
  begin
    Result.Tags            := new Tag_Array_T'(Tags);
    Result.Translate_Table := new Tp.Translate_Table
      (1 .. Tag_Number);
    return Result;
  end Create;

  -----------------------------------------------------------------------------

  procedure Free (Object : in out Reference_T)
  is
    procedure Liberate
      is new Ada.Unchecked_Deallocation (Object => Object_T,
                                         Name   => Reference_T);
  begin
    Liberate (Object);
  end Free;

  -----------------------------------------------------------------------------

  function Get_Template_Directory_Name
    (Self : in Object_T)
    return String
    is (Self.Template_Directory_Name.all);

  -----------------------------------------------------------------------------

  function Get_Template_File_Name
    (Self : in Object_T)
    return String
    is (Self.Template_File_Name.all);

  -----------------------------------------------------------------------------

  function Has_Tag
    (Self     : in Object_T;
     Tag_Name : in String)
    return Boolean
  is
  begin
    if Self.Tag_Names = null then
      return False;
    end if;

    for Index in Self.Tag_Names'Range loop
      if Self.Tag_Names (Index).all = Tag_Name then
        return True;
      end if;
    end loop;

    return False;
  end Has_Tag;

  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     String)
  is
    Pos : constant Positive := Self.Tag_Position (Tag_Name);
  begin
    Tp.Append (Self.Tags (Pos), Value);
  end Add_Value;

  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     Integer)
  is
    Pos : constant Positive := Self.Tag_Position (Tag_Name);
  begin
    Tp.Append (Self.Tags (Pos), Value);
  end Add_Value;

  -----------------------------------------------------------------------------

  procedure Add_Value
    (Self     : in out Object_T;
     Tag_Name : in     String;
     Value    : in     Boolean)
  is
    Pos : constant Positive := Self.Tag_Position (Tag_Name);
  begin
    Tp.Append (Self.Tags (Pos), Value);
  end Add_Value;

  -----------------------------------------------------------------------------

  procedure Out_Of_Bound
    (Tag_Name    : in String;
     Index, Size : in Natural);

  function Get_Value
    (Self     : in Object_T;
     Tag_Name : in String;
     Index    : in Positive)
    return String
  is
    Pos  : constant Integer := Self.Tag_Position (Tag_Name);
    Tag  : constant Tp.Tag  := Self.Tags (Pos);
    Size : constant Natural := Templates_Parser.Size (Tag);
  begin
    if Size < Index then
      Out_Of_Bound (Tag_Name, Index, Size);
    end if;

    return Templates_Parser.Item (Tag, Index);
  end Get_Value;

  procedure Out_Of_Bound
    (Tag_Name    : in String;
     Index, Size : in Natural)
  is
  begin
    raise Constraint_Error with "asked" & Index'Image &
      "th value for tag """ &
      Tag_Name & """, which has only" & Size'Image & " values";
  end Out_Of_Bound;

  -----------------------------------------------------------------------------

  function File_Exists
    (Self : in Object_T'Class)
    return Boolean
  is
    Full_File_Name : constant String := File_IO.Compose
      (Self.Get_Template_Directory_Name, Self.Get_Template_File_Name);
  begin
    return Ada.Directories.Exists (Full_File_Name);
  end File_Exists;

  -----------------------------------------------------------------------------

  function To_String
    (Self : in Object_T)
    return String
  is
    Full_File_Name : constant String := File_IO.Compose
      (Self.Get_Template_Directory_Name, Self.Get_Template_File_Name);
  begin
    for I in 1 .. Self.Tag_Names'Length loop
      Self.Translate_Table (I) := Tp.Assoc
        (Self.Tag_Names (I).all,
         Self.Tags (I));
    end loop;

    return Tp.Parse (Filename          => Full_File_Name,
                     Translations      => Self.Translate_Table.all,
                     Keep_Unknown_Tags => True);
  end To_String;

  -----------------------------------------------------------------------------

  function Number_Of_Elements_In_Tag
    (Self     : in Object_T;
     Tag_Name : in String)
    return Natural
  is
    Pos : constant Integer := Self.Tag_Position (Tag_Name);
  begin
    return Tp.Size (Self.Tags (Pos));
  end Number_Of_Elements_In_Tag;

  -----------------------------------------------------------------------------

  function Tag_Position
    (Self     : in Object_T;
     Tag_Name : in String)
    return Positive
  is
    Found    : Boolean  := False;
    Position : Positive := 1;

    Has_Some_Tags : constant Boolean := Self.Tag_Names'Length > 0;
  begin
    if Has_Some_Tags then

   Look_Tag:
      while Position <= Self.Tag_Names'Last
      loop
        if Self.Tag_Names (Position).all = Tag_Name then
          Found := True;
          exit Look_Tag;
        else
          Position := Position + 1;
        end if;
      end loop Look_Tag;

    end if;

    if not Found then
      raise Tag_Not_Found with "tag """ & Tag_Name & """ not found";
    end if;

    return Position;
  end Tag_Position;

end Templates;
