module Language.VHDL.Syntax where

--------------------------------------------------------------------------------
--
--                                   -- 1 --
--
--                      Design entities and configurations
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 1.1 Entiity Declarations
--------------------------------------------------------------------------------
{-
    entity_declaration ::=
      ENTITY identifier IS
        entity_header
        entity_declarative_part
      [ BEGIN
        entity_statement_part ]
      END [ ENTITY ] [ entity_simple_name ] ;
-}

data EntityDeclaration = EntityDeclaration {
    entity_identifier         :: Identifier
  , entity_header             :: EntityHeader
  , entity_declarative_part   :: EntityDeclarativePart
  , entity_statement_part     :: Maybe EntityStatementPart
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.1.1 Entity haeder
{-
    entity_header ::=
      [ formal_generic_clause ]
      [ formal_port_clause ]

    generic_clause ::= GENERIC ( generic_list ) ;
    port_clause    ::= PORT ( port_list ) ;
-}

data EntityHeader = EntityHeader {
    formal_generic_clause     :: Maybe GenericClause
  , formal_port_clause        :: Maybe PortClause
  }
  deriving (Eq, Show)

data GenericClause = GenericClause GenericList
  deriving (Eq, Show)

data PortClause    = PortClause    PortList
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 1.1.1.1 Generics
{-
    generic_list ::= generic_interface_list
-}

type GenericList = InterfaceList

--------------------------------------------------------------------------------
-- *** 1.1.1.2 Ports
{-
    port_list ::= port_interface_list
-}

type PortList = InterfaceList

--------------------------------------------------------------------------------
-- ** 1.1.2 Entity declarative part
{-
    entity_declarative_part ::=
      { entity_declarative_item }

    entity_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}

type EntityDeclarativePart = [EntityDeclarativeItem]

data EntityDeclarativeItem =
    EDISubprogDecl  SubprogramDeclaration
  | EDISubprogBody  SubprogramBody
  | EDIType         TypeDeclaration
  | EDISubtype      SubtypeDeclaration
  | EDIConstant     ConstantDeclaration
  | EDISignal       SignalDeclaration
  | EDIShared       VariableDeclaration
  | EDIFile         FileDeclaration
  | EDIAlias        AliasDeclaration
  | EDIAttrDecl     AttributeDeclaration
  | EDIAttrSpec     AttributeSpecification
  | EDIDiscSpec     DisconnectionSpecification
  | EDIUseClause    UseClause
  | EDIGroupTemp    GroupTemplateDeclaration
  | EDIGroup        GroupDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.1.3 Entity statement part
{-
    entity_statement_part ::=
      { entity_statement }

    entity_statement ::=
        concurrent_assertion_statement
      | passive_concurrent_procedure_call_statement
      | passive_process_statement
-}

type EntityStatementPart = [EntityStatement]

data EntityStatement =
    ESConcAssert   ConcurrentAssertionStatement
  | ESPassiveConc  ConcurrentProcedureCallStatement
  | ESPassiveProc  ProcessStatement
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 1.2 Arcitecture bodies
--------------------------------------------------------------------------------
{-
    architecture_body ::=
      ARCHITECTURE identifier OF entity_name IS
        architecture_declarative_part
      BEGIN
	architecture_statement_part
      END [ ARCHITECTURE ] [ architecture_simple_name ] ;
-}

data ArchitectureBody = ArchitectureBody {
    archi_identifier       :: Identifier
  , archi_entity_name      :: Name
  , archi_declarative_part :: ArchitectureDeclarativePart
  , archi_statement_part   :: ArchitectureStatementPart
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.2.1 Architecture declarative part
{-
    architecture_declarative_part ::=
      { block_declarative_item }

    block_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | component_declaration
      | attribute_declaration
      | attribute_specification
      | configuration_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}

type ArchitectureDeclarativePart = [BlockDeclarativeItem]

data BlockDeclarativeItem =
    BDISubprogDecl  SubprogramDeclaration
  | BDISubprogBody  SubprogramBody
  | BDIType         TypeDeclaration
  | BDISubtype      SubtypeDeclaration
  | BDIConstant     ConstantDeclaration
  | BDISignal       SignalDeclaration
  | BDIShared       VariableDeclaration
  | BDIFile         FileDeclaration
  | BDIAlias        AliasDeclaration
  | BDIComp         ComponentDeclaration
  | BDIAttrDecl     AttributeDeclaration
  | BDIAttrSepc     AttributeSpecification
  | BDIConfigSepc   ConfigurationSpecification
  | BDIDisconSpec   DisconnectionSpecification
  | BDIUseClause    UseClause
  | BDIGroupTemp    GroupTemplateDeclaration
  | BDIGroup        GroupDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.2.2 Architecture statement part
{-
    architecture_statement_part ::=
      { concurrent_statement }
-}

type ArchitectureStatementPart = [ConcurrentStatement]

--------------------------------------------------------------------------------
-- * 1.3 Configuration declarations
--------------------------------------------------------------------------------
{-
    configuration_declaration ::=
      CONFIGURATION identifier OF entity_name IS
        configuration_declarative_part
	block_configuration
      END [ CONFIGURATION ] [ configuration_simple_name ] ;

    configuration_declarative_part ::=
      { configuration_declarative_item }

    configuration_declarative_item ::=
	use_clause
      | attribute_specification
      | group_declaration
-}

data ConfigurationDeclaration = ConfigurationDeclaration {
    config_identifier          :: Identifier
  , config_entity_name         :: Name
  , config_declarative_part    :: ConfigurationDeclarativePart
  , config_block_configuration :: BlockConfiguration
  }
  deriving (Eq, Show)

type ConfigurationDeclarativePart = [ConfigurationDeclarativeItem]

data ConfigurationDeclarativeItem =
    CDIUse       UseClause
  | CDIAttrSpec  AttributeSpecification
  | CDIGroup     GroupDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.3.1 Block configuration
{-
    block_configuration ::=
      FOR block_specification
        { use_clause }
	{ configuration_item }
      END FOR ;

    block_specification ::=
        architecture_name
      | block_statement_label
      | generate_statement_label [ ( index_specification ) ]

    index_specification ::=
        discrete_range
      | static_expression

    configuration_item ::=
        block_configuration
      | component_configuration
-}

data BlockConfiguration = BlockConfiguration {
    block_specification      :: BlockSpecification
  , block_use_clause         :: [UseClause]
  , block_configuration_item :: [ConfigurationItem]
  }
  deriving (Eq, Show)

data BlockSpecification =
    BSArch  Name
  | BSBlock Label
  | BSGen   Label
  deriving (Eq, Show)

data IndexSpecification =
    ISRange DiscreteRange
  | ISExp   Expression
  deriving (Eq, Show)

data ConfigurationItem  =
    CIBlock BlockConfiguration
  | CIComp  ComponentConfiguration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 1.3.2 Component configuration

{-
    component_configuration ::=
      FOR component_specification
        [ binding_indication ; ]
	[ block_configuration ]
      END FOR ;
-}

data ComponentConfiguration = ComponentConfiguration {
    comp_specification       :: ComponentSpecification
  , comp_binding_indication  :: Maybe BindingIndication
  , comp_block_configuration :: Maybe BlockConfiguration
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--
--                                   -- 2 --
--
--                           Subprograms and packages
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 2.1 Subprogram declarations
--------------------------------------------------------------------------------
{-
    subprogram_declaration ::=
      subprogram_specification ;

    subprogram_specification ::=
      PROCEDURE designator [ ( formal_parameter_list ) ]
      | [ PURE | IMPURE ] FUNCTION designator [ ( formal_parameter_list ) ]
        RETURN type_mark

    designator ::= identifier | operator_symbol

    operator_symbol ::= string_literal
-}

type SubprogramDeclaration   = SubprogramSpecification

data SubprogramSpecification =
    SubprogramProcedure {
      subproc_designator            :: Designator
    , subproc_formal_parameter_list :: Maybe FormalParameterList
    }
  | SubprogramFunction {
      subfun_purity                 :: Maybe Bool
    , subfun_designator             :: Designator
    , subfun_formal_parameter_list  :: Maybe FormalParameterList
    , subfun_type_mark              :: TypeMark
    }
  deriving (Eq, Show)

data Designator =
    DId Identifier
  | DOp OperatorSymbol
  deriving (Eq, Show)

type OperatorSymbol = StringLiteral

--------------------------------------------------------------------------------
-- ** 2.1.1 Formal parameters
{-
    formal_parameter_list ::= parameter_interface_list
-}

type FormalParameterList = InterfaceList

--------------------------------------------------------------------------------
-- *** 2.1.1.1 Constant and variable parameters

-- properties ... todo

--------------------------------------------------------------------------------
-- *** 2.1.1.2 Signal parameter

-- properties ... todo

--------------------------------------------------------------------------------
-- *** 2.1.1.3 File parameter

-- properties ... todo

--------------------------------------------------------------------------------
-- * 2.2 Subprogram bodies
--------------------------------------------------------------------------------
{-
    subprogram_body ::=
      subprogram_specification IS
        subprogram_declarative_part
      BEGIN
	subprogram_statement_part
      END [ subprogram_kind ] [ designator ] ;

    subprogram_declarative_part ::=
      { subprogram_declarative_item }

    subprogram_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | variable_declaration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | use_clause
      | group_template_declaration
      | group_declaration


    subprogram_statement_part ::=
      { sequential_statement }

    subprogram_kind ::= PROCEDURE | FUNCTION
-}

data SubprogramBody = SubprogramBody {
    subprog_specification    :: SubprogramSpecification
  , subprog_declarative_part :: SubprogramDeclarativePart
  , subprog_statement_part   :: SubprogramStatementPart
  , subprog_kind             :: Maybe SubprogramKind
  , subprog_designator       :: Maybe Designator
  }
  deriving (Eq, Show)

type SubprogramDeclarativePart = [SubprogramDeclarativeItem]

data SubprogramDeclarativeItem =
    SDISubprogDecl  SubprogramDeclaration
  | SDISubprogBody  SubprogramBody
  | SDIType         TypeDeclaration
  | SDISubtype      SubtypeDeclaration
  | SDIConstant     ConstantDeclaration
  | SDIVariable     VariableDeclaration
  | SDIFile         FileDeclaration
  | SDIAlias        AliasDeclaration
  | SDIAttrDecl     AttributeDeclaration
  | SDIAttrSepc     AttributeSpecification
  | SDIUseClause    UseClause
  | SDIGroupTemp    GroupTemplateDeclaration
  | SDIGroup        GroupDeclaration
  deriving (Eq, Show)
    
type SubprogramStatementPart   = [SequentialStatement]

data SubprogramKind            = Procedure | Function
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 2.3 Subprogram overloading

-- properties ... todo

--------------------------------------------------------------------------------
-- ** 2.3.1 Operator overloading

-- properties ... todo

--------------------------------------------------------------------------------
-- ** 2.3.2 Signatures
{-
    signature ::= [ [ type_mark { , type_mark } ] [ RETURN type_mark ] ]
-}

data Signature = Signature (Maybe (Maybe [TypeMark], Maybe TypeMark))
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 2.4 Resolution functions

-- properties ... todo

--------------------------------------------------------------------------------
-- * 2.5 Package declarations
{- 
    package_declaration ::=
      PACKAGE identifier IS
        package_declarative_part
      END [ PACKAGE ] [ package_simple_name ] ;

    package_declarative_part ::=
      { package_declarative_item }

    package_declarative_item ::=
        subprogram_declaration
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | component_declaration
      | attribute_declaration
      | attribute_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}

data PackageDeclaration = PackageDeclaration {
    packd_identifier       :: Identifier
  , packd_declarative_part :: PackageDeclarativePart
  }
  deriving (Eq, Show)

type PackageDeclarativePart = [PackageDeclarativeItem]

data PackageDeclarativeItem =
    PHDISubprogDecl  SubprogramDeclaration
  | PHDISubprogBody  SubprogramBody
  | PHDIType         TypeDeclaration
  | PHDISubtype      SubtypeDeclaration
  | PHDIConstant     ConstantDeclaration
  | PHDISignal       SignalDeclaration
  | PHDIShared       VariableDeclaration
  | PHDIFile         FileDeclaration
  | PHDIAlias        AliasDeclaration
  | PHDIComp         ComponentDeclaration
  | PHDIAttrDecl     AttributeDeclaration
  | PHDIAttrSpec     AttributeSpecification
  | PHDIDiscSpec     DisconnectionSpecification
  | PHDIUseClause    UseClause
  | PHDIGroupTemp    GroupTemplateDeclaration
  | PHDIGroup        GroupDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 2.6 Package bodies
{-
    package_body ::=
      PACKAGE  package_simple_name IS
        package_body_declarative_part
      END [ PACKAGE BODY ] [ package_simple_name ] ;

    package_body_declarative_part ::=
      { package_body_declarative_item }

    package_body_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | use_clause
      | group_template_declaration
      | group_declaration
-}

data PackageBody = PackageBody {
    packb_simple_name           :: SimpleName
  , packb_body_declarative_part :: PackageBodyDeclarativePart
  }
  deriving (Eq, Show)

type PackageBodyDeclarativePart = [PackageBodyDeclarativeItem]

data PackageBodyDeclarativeItem = 
    PBDISubprogDecl  SubprogramDeclaration
  | PBDISubprogBody  SubprogramBody
  | PBDIType         TypeDeclaration
  | PBDISubtype      SubtypeDeclaration
  | PBDIConstant     ConstantDeclaration
  | PBDIShared       VariableDeclaration
  | PBDIFile         FileDeclaration
  | PBDIAlias        AliasDeclaration
  | PBDIUseClause    UseClause
  | PBDIGroupTemp    GroupTemplateDeclaration
  | PBDIGroup        GroupDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 2.7 Conformance rules

-- properties ... todo


--------------------------------------------------------------------------------
--
--                                   -- 3 --
--
--                                    Types
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 3.1 Scalar types
{-
    scalar_type_definition ::=
	enumeration_type_definition
      | integer_type_definition
      | floating_type_definition
      | physical_type_definition

    range_constraint ::= RANGE range

    range ::=
        range_attribute_name
      | simple_expression direction simple_expression

    direction ::= TO | DOWNTO
-}

data ScalarTypeDefinition =
    ScalarEnum  EnumerationTypeDefinition
  | ScalarInt   IntegerTypeDefinition
  | ScalarFloat FloatingTypeDefinition
  | ScalarPhys  PhysicalTypeDefinition
  deriving (Eq, Show)

data RangeConstraint = RangeConstraint Range
  deriving (Eq, Show)

data Range =
    RAttr   AttributeName
  | RSimple {
      range_lower :: SimpleExpression
    , range_dir   :: Direction
    , range_upper :: SimpleExpression
    }
  deriving (Eq, Show)

data Direction = To | DownTo
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 3.1.1 Enumeration types
{-
    enumeration_type_definition ::=
      ( enumeration_literal { , enumeration_literal } )

    enumeration_literal ::= identifier | character_literal
-}

data EnumerationTypeDefinition = EnumerationTypeDefinition [EnumerationLiteral]
  deriving (Eq, Show)

data EnumerationLiteral =
    EId   Identifier
  | EChar CharacterLiteral
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 3.1.1.1 Predefined enumeration types

-- predefined ... todo

--------------------------------------------------------------------------------
-- ** 3.1.2 Integer types
{-
    integer_type_definition ::= range_constraint
-}

type IntegerTypeDefinition = RangeConstraint

--------------------------------------------------------------------------------
-- *** 3.1.2.1 Predefined integer types

-- predefined ... todo

--------------------------------------------------------------------------------
-- ** 3.1.3 Physical types
{-
    physical_type_definition ::=
      range_constraint
        UNITS
	  primary_unit_declaration
	  { secondary_unit_declaration }
	END UNITS [ physical_type_simple_name ]

    primary_unit_declaration ::= identifier ;

    secondary_unit_declaration ::= identifier = physical_literal ;

    physical_literal ::= [ abstract_literal ] unit_name
-}

data PhysicalTypeDefinition = PhysicalTypeDefinition {
    physd_range_constraint           :: RangeConstraint
  , physd_primary_unit_declaration   :: PrimaryUnitDeclaration
  , physd_secondary_unit_declaration :: [SecondaryUnitDeclaration]
  , physd_simple_name                :: Maybe SimpleName
  }
  deriving (Eq, Show)

type PrimaryUnitDeclaration   = Identifier

data SecondaryUnitDeclaration = SecondaryUnitDeclaration Identifier PhysicalLiteral
  deriving (Eq, Show)

data PhysicalLiteral = PhysicalLiteral {
    physl_abstract_literal :: Maybe Literal
  , physl_unit_name        :: Name
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 3.1.3.1 Predefined physical types

-- predefined ... todo

--------------------------------------------------------------------------------
-- ** 3.1.4 Floating point types
{-
    floating_type_definition ::= range_constraint
-}

type FloatingTypeDefinition = RangeConstraint

--------------------------------------------------------------------------------
-- *** 3.1.4.1 Predefined floating point types

-- predefined ... todo

--------------------------------------------------------------------------------
-- * 3.2 Composite types
{-
    composite_type_definition ::=
        array_type_definition
      | record_type_definition
-}

data CompositeTypeDefinition =
    CTDArray  ArrayTypeDefinition
  | CTDRecord RecordTypeDefinition
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 3.2.1 Array types
{-
    array_type_definition ::=
	unconstrained_array_definition
      | constrained_array_definition

    unconstrained_array_definition ::=
      ARRAY ( index_subtype_definition { , index_subtype_definition } )
        OF element_subtype_indication

    constrained_array_definition ::=
      ARRAY index_constraint OF element_subtype_indication

    index_subtype_definition ::= type_mark RANGE <>

    index_constraint ::= ( discrete_range { , discrete_range } )

    discrete_range ::= discrete_subtype_indication | range
-}

data ArrayTypeDefinition =
    ArrU UnconstrainedArrayDefinition
  | ArrC ConstrainedArrayDefinition
  deriving (Eq, Show)

data UnconstrainedArrayDefinition = UnconstrainedArrayDefinition {
    arru_index_subtype_definition   :: [IndexSubtypeDefinition]
  , arru_element_subtype_indication :: SubtypeIndication
  }
  deriving (Eq, Show)

data ConstrainedArrayDefinition = ConstrainedArrayDefinition {
    arrc_index_constraint   :: IndexConstraint
  , arrc_subtype_indication :: SubtypeIndication
  }
  deriving (Eq, Show)

data IndexSubtypeDefinition = IndexSubtypeDefinition TypeMark
  deriving (Eq, Show)

data IndexConstraint = IndexConstraint [DiscreteRange]
  deriving (Eq, Show)

data DiscreteRange =
    DRSub   SubtypeIndication
  | DRRange Range
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 3.2.1.1 Index constraints and discrete ranges

-- constraints ... todo

--------------------------------------------------------------------------------
-- *** 3.2.1.2 Predefined array types

-- predefined ... todo

--------------------------------------------------------------------------------
-- ** 3.2.2 Record types
{-
    record_type_definition ::=
      RECORD
        element_declaration
	{ element_declaration }
      END RECORD [ record_type_simple_name ]

    element_declaration ::=
      identifier_list : element_subtype_definition ;

    identifier_list ::= identifier { , identifier }

    element_subtype_definition ::= subtype_indication
-}

data RecordTypeDefinition = RecordTypeDefinition {
    rectd_element_declaration :: [ElementDeclaration]
  , rectd_type_simple_name    :: Maybe SimpleName
  }
  deriving (Eq, Show)

data ElementDeclaration = ElementDeclaration {
    elemd_identifier_list    :: IdentifierList
  , elemd_subtype_definition :: ElementSubtypeDefinition
  }
  deriving (Eq, Show)

type IdentifierList           = [Identifier]

type ElementSubtypeDefinition = SubtypeIndication

--------------------------------------------------------------------------------
-- * 3.3 Access types
{-
    access_type_definition ::= ACCESS subtype_indication
-}

data AccessTypeDefinition = AccessTypeDefinition SubtypeIndication
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 3.3.1 Incomplete type declarations
{-
    incomplete_type_declaration ::= TYPE identifier ;
-}

data IncompleteTypeDeclaration = IncompleteTypeDeclaration Identifier
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 3.3.2 Allocation and deallocation of objects

-- ?

--------------------------------------------------------------------------------
-- * 3.4 File types
{-
    file_type_definition ::= FILE OF type_mark
-}

data FileTypeDefinition = FileTypeDefinition TypeMark
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 3.4.1 File operations

-- ?

--------------------------------------------------------------------------------
-- * 3.5 Protected types

-- I'll skip these for now..

--------------------------------------------------------------------------------
--
--                                   -- 4 --
--
--                                Declarations
--
--------------------------------------------------------------------------------
{-
    declaration ::=
        type_declaration
      | subtype_declaration
      | object_declaration
      | interface_declaration
      | alias_declaration
      | attribute_declaration
      | component_declaration
      | group_template_declaration
      | group_declaration
      | entity_declaration
      | configuration_declaration
      | subprogram_declaration
      | package_declaration
-}

data Declaration = 
    DType          TypeDeclaration
  | DSubtype       SubtypeDeclaration
  | DObject        ObjectDeclaration
  | DAlias         AliasDeclaration
  | DComponent     ComponentDeclaration
  | DAttribute     AttributeDeclaration
  | DGroupTemplate GroupTemplateDeclaration
  | DGroup         GroupDeclaration
  | DEntity        EntityDeclaration
  | DConfiguration ConfigurationDeclaration
  | DSubprogram    SubprogramDeclaration
  | DPackage       PackageDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.1 Type declarations
{-
    type_declaration ::=
        full_type_declaration
      | incomplete_type_declaration

    full_type_declaration ::=
      TYPE identifier IS type_definition ;

    type_definition ::=
        scalar_type_definition
      | composite_type_definition
      | access_type_definition
      | file_type_definition
      | protected_type_definition  -- missing from ref. manual
-}

data TypeDeclaration = TDFull FullTypeDeclaration | TDPartial IncompleteTypeDeclaration
  deriving (Eq, Show)

data FullTypeDeclaration = FullTypeDeclaration {
    ftd_identifier      :: Identifier
  , ftd_type_definition :: TypeDefinition
  }
  deriving (Eq, Show)

data TypeDefinition =
    TDScalar       ScalarTypeDefinition
  | TDComposite CompositeTypeDefinition
  | TDAccess       AccessTypeDefinition
  | TDFile           FileTypeDefinition
--  | TDProt      ProtectedTypeDefinition
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.2 Subtype declarations
{-
    subtype_declaration ::=
      SUBTYPE identifier IS subtype_indication ;

    subtype_indication ::=
      [ resolution_function_name ] type_mark [ constraint ]

    type_mark ::=
        type_name
      | subtype_name

    constraint ::=
        range_constraint
      | index_constraint
-}

data SubtypeDeclaration = SubtypeDeclaration {
    sd_identifier               :: Identifier
  , sd_indication               :: SubtypeIndication
  }
  deriving (Eq, Show)

data SubtypeIndication = SubtypeIndication {
    si_resolution_function_name :: Maybe Name
  , si_type_mark                :: TypeMark
  , si_constraint               :: Maybe Constraint
  }
  deriving (Eq, Show)

data TypeMark   = TMType Name | TMSubtype Name
  deriving (Eq, Show)

data Constraint = CRange RangeConstraint | CIndex IndexConstraint
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.3 Objects

--------------------------------------------------------------------------------
-- ** 4.3.1 Object declarations
{-
    object_declaration ::=
        constant_declaration
      | signal_declaration
      | variable_declaration
      | file_declaration
-}

data ObjectDeclaration =
    ObjConst ConstantDeclaration
  | ObjSig     SignalDeclaration
  | ObjVar   VariableDeclaration
  | ObjFile      FileDeclaration
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.1.1 Constant declarations
{-
    constant_declaration ::=
      CONSTANT identifier_list : subtype_indication [ := expression ] ;
-}

data ConstantDeclaration = ConstantDeclaration {
    const_identifier_list    :: IdentifierList
  , const_subtype_indication :: SubtypeIndication
  , const_expression         :: Maybe Expression
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.1.2 Signal declarations
{-
    signal_declaration ::=
      SIGNAL identifier_list : subtype_indication [ signal_kind ] [ := expression ] ;

    signal_kind ::= REGISTER | BUS
-}

data SignalDeclaration = SignalDeclaration {
    signal_identifier_list    :: IdentifierList
  , signal_subtype_indication :: SubtypeIndication
  , signal_kind               :: Maybe SignalKind
  , signal_expression         :: Maybe Expression
  }
  deriving (Eq, Show)

data SignalKind = Register | Bus
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.1.3 Variable declarations
{-
    variable_declaration ::=
      [ SHARED ] VARIABLE identifier_list : subtype_indication [ := expression ] ;
-}

data VariableDeclaration = VariableDeclaration {
    var_shared             :: Bool
  , var_identifier_list    :: IdentifierList
  , var_subtype_indication :: SubtypeIndication
  , var_expression         :: Maybe Expression
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.1.4 File declarations
{-
    file_declaration ::=
      FILE identifier_list : subtype_indication [ file_open_information ] ;

    file_open_information ::=
      [ OPEN file_open_kind_expression ] IS file_logical_name

    file_logical_name ::= string_expression
-}

data FileDeclaration = FileDeclaration {
    fd_identifier_list      :: IdentifierList
  , fd_subtype_indication   :: SubtypeIndication
  , fd_open_information     :: Maybe FileOpenInformation
  }
  deriving (Eq, Show)

data FileOpenInformation = FileOpenInformation {
    foi_open_kind_expression :: Maybe Expression
  , foi_logical_name         :: FileLogicalName
  }
  deriving (Eq, Show)

type FileLogicalName = Expression

--------------------------------------------------------------------------------
-- ** 4.3.2 Interface declarations
{-
    interface_declaration ::=
        interface_constant_declaration
      | interface_signal_declaration
      | interface_variable_declaration
      | interface_file_declaration

    interface_constant_declaration ::=
      [ CONSTANT ] identifier_list : [ IN ] subtype_indication [ := static_expression ]

    interface_signal_declaration ::=
      [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ] [ := static_expression ]

    interface_variable_declaration ::=
      [ VARIABLE ] identifier_list : [ mode ] subtype_indication [ := static_expression ]

    interface_file_declaration ::=
	FILE identifier_list : subtype_indication

    mode ::= IN | OUT | INOUT | BUFFER | LINKAGE
-}

data InterfaceDeclaration
  = InterfaceConstantDeclaration {
        idecl_identifier_list     :: IdentifierList
      , iconst_subtype_indication :: SubtypeIndication
      , iconst_static_expression  :: Maybe Expression
    }
  | InterfaceSignalDeclaration {
        idecl_identifier_list     :: IdentifierList
      , isig_mode                 :: Maybe Mode
      , isig_subtype_indication   :: SubtypeIndication
      , isig_bus                  :: Bool
      , isig_static_expression    :: Maybe Expression
    }
  | InterfaceVariableDeclaration {
        idecl_identifier_list     :: IdentifierList
      , ivar_mode                 :: Maybe Mode
      , ivar_subtype_indication   :: SubtypeIndication
      , ivar_static_expression    :: Maybe Expression
    }
  | InterfaceFileDeclaration {
        idecl_identifier_list     :: IdentifierList
      , ifile_subtype_indication  :: SubtypeIndication
    }
  deriving (Eq, Show)

data Mode = In | Out | InOut | Buffer | Linkage
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.2.1 Interface lists
{-
    interface_list ::= interface_element { ; interface_element }

    interface_element ::= interface_declaration
-}

data InterfaceList    = InterfaceList [InterfaceElement]
  deriving (Eq, Show)

type InterfaceElement = InterfaceDeclaration

--------------------------------------------------------------------------------
-- *** 4.3.2.2 Association lists
{-
    association_element ::=
      [ formal_part => ] actual_part

    association_list ::=
      association_element { , association_element }

    formal_designator ::=
        generic_name
      | port_name
      | parameter_name

    formal_part ::=
        formal_designator
      | function_name ( formal_designator )
      | type_mark ( formal_designator )

    actual_designator ::=
	expression
      | signal_name
      | variable_name
      | file_name
      | OPEN

    actual_part ::=
        actual_designator
      | function_name ( actual_designator )
      | type_mark ( actual_designator )
-}

data AssociationElement = AssociationElement {
    assoc_formal_part :: Maybe FormalPart
  , assoc_actual_part :: ActualPart
  }
  deriving (Eq, Show)

data AssociationList = AssociationList [AssociationElement]
  deriving (Eq, Show)

data FormalDesignator =
    FDGeneric   Name
  | FDPort      Name
  | FDParameter Name
  deriving (Eq, Show)

data FormalPart =
    FPDesignator          FormalDesignator
  | FPFunction   Name     FormalDesignator
  | FPType       TypeMark FormalDesignator
  deriving (Eq, Show)

data ActualDesignator =
    ADExpression  Expression
  | ADSignal      Name
  | ADVariable    Name
  | ADFile        Name
  | ADOpen
  deriving (Eq, Show)

data ActualPart =
    APDesignator          ActualDesignator
  | APFunction   Name     ActualDesignator
  | APType       TypeMark ActualDesignator
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 4.3.3 Alias declarations
{-
    alias_declaration ::=
      ALIAS alias_designator [ : subtype_indication ] IS name [ signature ] ;

    alias_designator ::= identifier | character_literal | operator_symbol
-}

data AliasDeclaration = AliasDeclaration {
    alias_designator         :: AliasDesignator
  , alias_subtype_indication :: Maybe SubtypeIndication
  , alias_name               :: Name
  , alias_signature          :: Maybe Signature
  }
  deriving (Eq, Show)

data AliasDesignator =
    ADIdentifier Identifier
  | ADCharacter  CharacterLiteral
  | ADOperator   OperatorSymbol
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 4.3.3.1 Object aliases

--------------------------------------------------------------------------------
-- *** 4.3.3.2 Nonobject aliases

--------------------------------------------------------------------------------
-- * 4.4 Attribute declarations
{-
    attribute_declaration ::=
      ATTRIBUTE identifier : type_mark ;
-}

data AttributeDeclaration = AttributeDeclaration {
    attr_identifier :: Identifier
  , attr_type_marke :: TypeMark
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.5 Component declarations
{-
    component_declaration ::=
      COMPONENT identifier [ IS ]
        [ local_generic_clause ]
	[ local_port_clause ]
      END COMPONENT [ component_simple_name ] ;
-}

data ComponentDeclaration = ComponentDeclaration {
    comp_identifier           :: Identifier
  , comp_local_generic_clause :: Maybe GenericClause
  , comp_local_port_clause    :: Maybe PortClause
  , comp_simple_name          :: Maybe SimpleName
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.6 Group template declarations
{-
    group_template_declaration ::=
      GROUP identifier IS ( entity_class_entry_list ) ;

    entity_class_entry_list ::=
      entity_class_entry { , entity_class_entry }

    entity_class_entry ::= entity_class [ <> ]
-}

data GroupTemplateDeclaration = GroupTemplateDeclaration {
    gtd_identifier              :: Identifier
  , gtd_entity_class_entry_list :: EntityClassEntryList
  }
  deriving (Eq, Show)

type EntityClassEntryList = [EntityClassEntry]

data EntityClassEntry = EntityClassEntry {
    entc_entity_class :: EntityClass
  , entc_multiple     :: Bool
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 4.7 Group declarations
{-
    group_declaration ::=
      GROUP identifier : group_template_name ( group_constituent_list ) ;

    group_constituent_list ::= group_constituent { , group_constituent }

    group_constituent ::= name | character_literal
-}

data GroupDeclaration = GroupDeclaration {
    group_identifier       :: Identifier
  , group_template_name    :: Name
  , group_constituent_list :: GroupConstituentList
  }
  deriving (Eq, Show)

type GroupConstituentList = [GroupConstituent]

data GroupConstituent =
    GCName Name
  | GCChar CharacterLiteral
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--
--                                   -- 5 --
--
--                               Specifications
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 5.1 Attribute specification
{-
    attribute_specification ::=
      ATTRIBUTE attribute_designator OF entity_specification IS expression ;

    entity_specification ::=
      entity_name_list : entity_class

    entity_class ::=
        ENTITY     | ARCHITECTURE  | CONFIGURATION
      | PROCEDURE  | FUNCTION	   | PACKAGE
      | TYPE       | SUBTYPE	   | CONSTANT
      | SIGNAL     | VARIABLE      | COMPONENT
      | LABEL	   | LITERAL       | UNITS
      | GROUP	   | FILE

    entity_name_list ::=
	entity_designator { , entity_designator }
      | OTHERS
      | ALL

    entity_designator ::= entity_tag [ signature ]

    entity_tag ::= simple_name | character_literal | operator_symbol
-}

data AttributeSpecification = AttributeSpecification {
    as_attribute_designator :: AttributeDesignator
  , as_entity_specification :: EntitySpecification
  , as_expression           :: Expression
  }
  deriving (Eq, Show)

data EntitySpecification = EntitySpecification {
    es_entity_name_list     :: EntityNameList
  , es_entity_class         :: EntityClass
  }
  deriving (Eq, Show)

data EntityClass =
    ENTITY     | ARCHITECTURE  | CONFIGURATION
  | PROCEDURE  | FUNCTION      | PACKAGE
  | TYPE       | SUBTYPE       | CONSTANT
  | SIGNAL     | VARIABLE      | COMPONENT
  | LABEL      | LITERAL       | UNITS
  | GROUP      | FILE
  deriving (Eq, Show)

data EntityNameList =
    ENLDesignators [EntityDesignator]
  | ENLOthers
  | ENLAll
  deriving (Eq, Show)

data EntityDesignator = EntityDesignator {
    ed_entity_tag :: EntityTag
  , ed_signature  :: Maybe Signature
  }
  deriving (Eq, Show)

data EntityTag =
    ETName SimpleName
  | ETChar CharacterLiteral
  | ETOp   OperatorSymbol
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 5.2 Configuration specification
{-
    configuration_specification ::=
      FOR component_specification binding_indication ;

    component_specification ::=
      instantiation_list : component_name

    instantiation_list ::=
	instantiation_label { , instantiation_label }
      | OTHERS
      | ALL

-}

data ConfigurationSpecification = ConfigurationSpecification {
    cs_component_specification :: ComponentSpecification
  , cs_binding_indication      :: BindingIndication
  }
  deriving (Eq, Show)

data ComponentSpecification = ComponentSpecification {
    cs_instantiation_list      :: InstantiationList
  , cs_component_name          :: Name
  }
  deriving (Eq, Show)

data InstantiationList =
    ILLabels [Label]
  | ILOthers
  | ILAll
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 5.2.1 Binding indication
{-
    binding_indication ::=
      [ USE entity_aspect ]
      [ generic_map_aspect ]
      [ port_map_aspect ]
-}

data BindingIndication = BindingIndication {
    bi_entity_aspect      :: Maybe EntityAspect
  , bi_generic_map_aspect :: Maybe GenericMapAspect
  , bi_port_map_aspect    :: Maybe PortMapAspect
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 5.2.1.1 Entity aspect
{-
    entity_aspect ::=
        ENTITY entity_name [ ( architecture_identifier) ]
      | CONFIGURATION configuration_name
      | OPEN
-}

data EntityAspect =
    EAEntity Name (Maybe Identifier)
  | EAConfig Name
  | EAOpen
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 5.2.1.2 Generic map and port map aspects
{-
    generic_map_aspect ::=
      GENERIC MAP ( generic_association_list )

    port_map_aspect ::=
      PORT MAP ( port_association_list )
-}

data GenericMapAspect = GenericMapAspect AssociationList
  deriving (Eq, Show)

data PortMapAspect    = PortMapAspect    AssociationList
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 5.2.2 Default binding indication

-- defaults ... todo?

--------------------------------------------------------------------------------
-- * 5.3 Disconnection specification
{-
    disconnection_specification ::=
      DISCONNECT guarded_signal_specification AFTER time_expression ;

    guarded_signal_specification ::=
      guarded_signal_list : type_mark

    signal_list ::=
	signal_name { , signal_name }
      | OTHERS
      | ALL
-}

data DisconnectionSpecification = DisconnectionSpecification {
    ds_guarded_signal_specification :: GuardedSignalSpecification
  , ds_time_expression              :: Expression
  }
  deriving (Eq, Show)

data GuardedSignalSpecification = GuardedSignalSpecification {
    gs_guarded_signal_list          :: SignalList
  , gs_type_mark                    :: TypeMark
  }
  deriving (Eq, Show)

data SignalList =
    SLName   [Name]
  | SLOthers
  | SLAll
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--
--                                   -- 6 --
--
--                                    Names
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 6.1 Names
{-
    name ::=
	simple_name
      | operator_symbol
      | selected_name
      | indexed_name
      | slice_name
      | attribute_name

    prefix ::=
        name
      | function_call
-}

data Name =
    NSimple SimpleName
  | NOp     OperatorSymbol
  | NSelect SelectedName
  | NIndex  IndexedName
  | NSlice  SliceName
  | NAttr   AttributeName
  deriving (Eq, Show)

data Prefix =
    PName Name
  | PFun  FunctionCall
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 6.2 Simple names
{-
    simple_name ::= identifier
-}

type SimpleName = Identifier

--------------------------------------------------------------------------------
-- * 6.3 Selected names
{-
    selected_name ::= prefix . suffix

    suffix ::=
        simple_name
      | character_literal
      | operator_symbol
      | ALL
-}

data SelectedName = SelectedName {
    sname_prefix :: Prefix
  , sname_suffix :: Suffix
  }
  deriving (Eq, Show)

data Suffix =
    SSimple SimpleName
  | SChar   CharacterLiteral
  | SOp     OperatorSymbol
  | SAll
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 6.4 Indexed names
{-
    indexed_name ::= prefix ( expression { , expression } )
-}

data IndexedName = IndexedName {
    iname_prefix     :: Prefix
  , iname_expression :: [Expression]
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 6.5 Slice names
{-
    slice_name ::= prefix ( discrete_range )
-}

data SliceName = SliceName {
    slice_prefix         :: Prefix
  , slice_discrete_range :: DiscreteRange
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 6.6 Attribute names
{-
    attribute_name ::=
      prefix [ signature ] ' attribute_designator [ ( expression ) ]

    attribute_designator ::= attribute_simple_name
-}

data AttributeName = AttributeName {
    aname_prefix               :: Prefix
  , aname_signature            :: Maybe Signature
  , aname_attribute_designator :: AttributeDesignator
  , aname_expression           :: Maybe Expression
  }
  deriving (Eq, Show)

type AttributeDesignator = SimpleName

--------------------------------------------------------------------------------
--
--                                   -- 7 --
--
--                                 Expression
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 7.1 Rules for expressions
{-
    expression ::=
	relation { AND relation }
      | relation { OR relation }
      | relation { XOR relation }
      | relation [ NAND relation ]
      | relation [ NOR relation ]
      | relation { XNOR relation }

    relation ::=
      shift_expression [ relational_operator shift_expression ]

    shift_expression ::=
      simple_expression [ shift_operator simple_expression ]

    simple_expression ::=
      [ sign ] term { adding_operator term }

    term ::=
      factor { multiplying_operator factor }

    factor ::=
	primary [ ** primary ]
      | ABS primary
      | NOT primary

    primary ::=
	name
      | literal
      | aggregate
      | function_call
      | qualified_expression
      | type_conversion
      | allocator
      | ( expression )
-}

data Expression =
    EAnd  [Relation]
  | EOr   [Relation]
  | EXor  [Relation]
  | ENand (Relation) (Maybe Relation)
  | ENor  (Relation) (Maybe Relation)
  | EXnor [Relation]
  deriving (Eq, Show)

data Relation         = Relation {
    relation_shift_expression :: ShiftExpression
  , relation_operator         :: Maybe (RelationalOperator, ShiftExpression)
  }
  deriving (Eq, Show)

data ShiftExpression  = ShiftExpression {
    shifte_simple_expression  :: SimpleExpression
  , shifte_shift_operator     :: Maybe (ShiftOperator, SimpleExpression)
  }
  deriving (Eq, Show)

data SimpleExpression = SimpleExpression {
    sexp_sign                 :: Maybe Sign
  , sexp_term                 :: Term
  , sexp_adding               :: [(AddingOperator, Term)]
  }
  deriving (Eq, Show)

data Term = Term {
    term_factor               :: Factor
  , term_multiplying          :: [(MultiplyingOperator, Factor)]
  }
  deriving (Eq, Show)

data Factor =
    FacPrim Primary (Maybe Primary)
  | FacAbs  Primary
  | FacNot  Primary
  deriving (Eq, Show)

data Primary =
    PrimName  Name
  | PrimLit   Literal
  | PrimAgg   Aggregate
  | PrimFun   FunctionCall
  | PrimQual  QualifiedExpression
  | PrimTCon  TypeConversion
  | PrimAlloc Allocator
  | PrimExp   Expression
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 7.2 Operators
{-
    logical_operator ::= AND | OR | NAND | NOR | XOR | XNOR

    relational_operator ::= = | /= | < | <= | > | >=

    shift_operator ::= SLL | SRL | SLA | SRA | ROL | ROR

    adding_operator ::= + | – | &

    sign ::= + | –

    multiplying_operator ::= * | / | MOD | REM

    miscellaneous_operator ::= ** | ABS | NOT
-}

data LogicalOperator       = And | Or | Nand | Nor | Xor | Xnor
  deriving (Eq, Show)

data RelationalOperator    = Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Eq, Show)

data ShiftOperator         = Sll | Srl | Sla | Sra | Rol | Ror
  deriving (Eq, Show)

data AddingOperator        = Plus | Minus | Concat
  deriving (Eq, Show)

data Sign                  = Identity | Negation
  deriving (Eq, Show)

data MultiplyingOperator   = Times | Div | Mod | Rem
  deriving (Eq, Show)

data MiscellaneousOperator = Exp | Abs | Not
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 7.2.1 Logical operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.2 Relational operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.3 Shift operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.4 Adding operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.5 Sign operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.6 Multiplying operators

-- ...

--------------------------------------------------------------------------------
-- ** 7.2.7 Miscellaneous operators

-- ...

--------------------------------------------------------------------------------
-- * 7.3 Operands

--------------------------------------------------------------------------------
-- ** 7.3.1 Literals
{-
    literal ::=
	numeric_literal
      | enumeration_literal
      | string_literal
      | bit_string_literal
      | NULL

    numeric_literal ::=
	abstract_literal
      | physical_literal
-}

data Literal =
    LitNum       NumericLiteral
  | LitEnum      EnumerationLiteral
  | LitString    StringLiteral
  | LitBitString BitStringLiteral
  | LitNull 
  deriving (Eq, Show)

data NumericLiteral =
    NLitAbstract AbstractLiteral
  | NLitPhysical PhysicalLiteral
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 7.3.2 Aggregates

{-
    aggregate ::=
      ( element_association { , element_association } )

    element_association ::=
      [ choices => ] expression

    choices ::= choice { | choice }

    choice ::=
        simple_expression
      | discrete_range
      | element_simple_name
      | OTHERS
-}

data Aggregate = Aggregate {
    agg_element_association :: [ElementAssociation]
  }
  deriving (Eq, Show)

data ElementAssociation = ElementAssociation {
    eassoc_choices'   :: Maybe Choices
  , eassoc_expression :: Expression
  }
  deriving (Eq, Show)

data Choices = Choices [Choice]
  deriving (Eq, Show)

data Choice =
    ChoiceSimple SimpleExpression
  | ChoiceRange  DiscreteRange
  | ChoiceName   SimpleName
  | ChoiceOthers
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 7.3.2.1 Record aggregates

-- ...

--------------------------------------------------------------------------------
-- *** 7.3.2.2 Array aggregates

-- ...

--------------------------------------------------------------------------------
-- ** 7.3.3 Function calls
{-
    function_call ::=
      function_name [ ( actual_parameter_part ) ]

    actual_parameter_part ::= parameter_association_list
-}

data FunctionCall = FunctionCall {
    fc_function_name         :: Name
  , fc_actual_parameter_part :: Maybe ActualParameterPart
  }
  deriving (Eq, Show)

type ActualParameterPart = AssociationList

--------------------------------------------------------------------------------
-- ** 7.3.4 Qualified expressions
{-
    qualified_expression ::=
	type_mark ' ( expression )
      | type_mark ' aggregate
-}

data QualifiedExpression =
    QualExp TypeMark Expression
  | QualAgg TypeMark Aggregate
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 7.3.5 Type conversions
{-
    type_conversion ::= type_mark ( expression )
-}

data TypeConversion = TypeConversion {
    type_mark  :: TypeMark
  , expression :: Expression
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 7.3.6 Allocators
{-
    allocator ::=
	NEW subtype_indication
      | NEW qualified_expression
-}

data Allocator =
    AllocSub  SubtypeIndication
  | AllocQual QualifiedExpression
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 7.4 Static expressions

--------------------------------------------------------------------------------
-- ** 7.4.1 Locally static primaries

-- ...

--------------------------------------------------------------------------------
-- ** 7.4.2 Globally static primaries

-- ...

--------------------------------------------------------------------------------
-- * 7.5 Universal expressions

-- ...


--------------------------------------------------------------------------------
--
--                                   -- 8 --
--
--                             Sequential statements
--
--------------------------------------------------------------------------------
{-
    sequence_of_statements ::= { sequential_statement }

    sequential_statement ::=
        wait_statement
      | assertion_statement
      | report_statement
      | signal_assignment_statement
      | variable_assignment_statement
      | procedure_call_statement
      | if_statement
      | case_statement
      | loop_statement
      | next_statement
      | exit_statement
      | return_statement
      | null_statement
-}

type SequenceOfStatements = [SequentialStatement]

data SequentialStatement =
    SWait      WaitStatement
  | SAssert    AssertionStatement
  | SReport    ReportStatement
  | SSignalAss SignalAssignmentStatement
  | SVarAss    VariableAssignmentStatement
  | SProc      ProcedureCallStatement
  | SIf        IfStatement
  | SCase      CaseStatement
  | SLoop      LoopStatement
  | SNext      NextStatement
  | SExit      ExitStatement
  | SReturn    ReturnStatement
  | SNull      NullStatement
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.1 Wait statement
{-
    wait_statement ::=
      [ label : ] WAIT [ sensitivity_clause ] [ condition_clause ] [ timeout_clause ] ;

    sensitivity_clause ::= ON sensitivity_list

    sensitivity_list ::= signal_name { , signal_name }

    condition_clause ::= UNTIL condition

    condition ::= boolean_expression

    timeout_clause ::= FOR time_expression
-}

data WaitStatement = WaitStatement
    (Maybe Label) (Maybe SensitivityClause) (Maybe ConditionClause) (Maybe TimeoutClause)
  deriving (Eq, Show)

data SensitivityClause = SensitivityClause SensitivityList
  deriving (Eq, Show)

data SensitivityList = SensitivityList [Name]
  deriving (Eq, Show)

data ConditionClause = ConditionClause Condition
  deriving (Eq, Show)

type Condition = Expression

data TimeoutClause = TimeoutClause Expression
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.2 Assertion statement
{-
    assertion_statement ::= [ label : ] assertion ;

    assertion ::=
      ASSERT condition
        [ REPORT expression ]
        [ SEVERITY expression ]
-}

data AssertionStatement = AssertionStatement
      (Maybe Label) Assertion
  deriving (Eq, Show)

data Assertion = Assertion
      Condition (Maybe Expression) (Maybe Expression)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.3 Report statement
{-
    report_statement ::=
      [ label : ]
        REPORT expression
          [ SEVERITY expression ] ;
-}

data ReportStatement = ReportStatement
      (Maybe Label) Expression (Maybe Expression)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.4 Signal assignment statement
{-
    signal_assignment_statement ::=
      [ label : ] target <= [ delay_mechanism ] waveform ;

    delay_mechanism ::=
        TRANSPORT
      | [ REJECT time_expression ] INERTIAL

    target ::=
        name
      | aggregate

    waveform ::=
        waveform_element { , waveform_element }
      | UNAFFECTED
-}

data SignalAssignmentStatement = SignalAssignmentStatement
      (Maybe Label) Target (Maybe DelayMechanism) Waveform
  deriving (Eq, Show)

data DelayMechanism =
    DMechTransport
  | DMechInertial  (Maybe Expression)
  deriving (Eq, Show)

data Target = 
    TargetName Name
  | TargetAgg  Aggregate
  deriving (Eq, Show)

data Waveform =
    WaveElem [WaveformElement]
  | WaveUnaffected
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 8.4.1 Updating a projected output waveform
{-
    waveform_element ::=
        value_expression [ AFTER time_expression ]
      | null [ AFTER time_expression ]
-}

data WaveformElement =
    WaveEExp  Expression (Maybe Expression)
  | WaveENull            (Maybe Expression)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.5 Variable assignment statement
{-
    variable_assignment_statement ::=
      [ label : ] target := expression ;
-}

data VariableAssignmentStatement = VariableAssignmentStatement
      (Maybe Label) Target Expression
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 8.5.1 Array variable assignments

-- ...

--------------------------------------------------------------------------------
-- * 8.6 Procedure call statement
{-
    procedure_call_statement ::= [ label : ] procedure_call ;

    procedure_call ::= procedure_name [ ( actual_parameter_part ) ]
-}

data ProcedureCallStatement = ProcedureCallStatement
      (Maybe Label) ProcedureCall
  deriving (Eq, Show)

data ProcedureCall = ProcedureCall
      Name (Maybe ActualParameterPart)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.7 If statement
{-
    if_statement ::=
      [ if_label : ]
        IF condition THEN
          sequence_of_statements
        { ELSEIF condition THEN
          sequence_of_statements }
        [ ELSE
          sequence_of_statements ]
        END IF [ if_label ] ;
-}

data IfStatement = IfStatement {
    if_label     :: Maybe Label
  , if_then      :: (Condition, SequenceOfStatements)
  , if_also      :: [(Condition, SequenceOfStatements)]
  , if_else      :: Maybe SequenceOfStatements
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.8 Case statement
{-
    case_statement ::=
      [ case_label : ]
        CASE expression IS
          case_statement_alternative
          { case_statement_alternative }
        END CASE [ case_label ] ;

    case_statement_alternative ::=
      WHEN choices =>
        sequence_of_statements
-}

data CaseStatement = CaseStatement {
    case_label        :: Maybe Label
  , case_expression   :: Expression
  , case_alternatives :: [CaseStatementAlternative]
  }
  deriving (Eq, Show)

data CaseStatementAlternative = CaseStatementAlternative Choices SequenceOfStatements
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.9 Loop statement
{-
    loop_statement ::=
      [ loop_label : ]
        [ iteration_scheme ] LOOP
          sequence_of_statements
        END LOOP [ loop_label ] ;

    iteration_scheme ::=
        WHILE condition
      | FOR loop_parameter_specification

    parameter_specification ::=
      identifier IN discrete_range
-}

data LoopStatement = LoopStatement {
    loop_label            :: Maybe Label
  , loop_iteration_scheme :: Maybe IterationScheme
  , loop_statements       :: SequenceOfStatements
  }
  deriving (Eq, Show)

data IterationScheme =
    IterWhile Condition
  | IterFor   ParameterSpecification
  deriving (Eq, Show)

data ParameterSpecification = ParameterSpecification {
    paramspec_identifier     :: Identifier
  , paramspec_discrete_range :: DiscreteRange
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.10 Next statement
{-
    next_statement ::=
      [ label : ] NEXT [ loop_label ] [ WHEN condition ] ;
-}

data NextStatement = NextStatement {
    next_label :: Maybe Label
  , next_loop  :: Maybe Label
  , next_when  :: Maybe Condition
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.11 Exit statement
{-
    exit_statement ::=
      [ label : ] EXIT [ loop_label ] [ WHEN condition ] ;
-}

data ExitStatement = ExitStatement {
    exit_label :: Maybe Label
  , exit_loop  :: Maybe Label
  , exit_when  :: Maybe Condition
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.12 Return statement
{-
    return_statement ::=
      [ label : ] RETURN [ expression ] ;
-}

data ReturnStatement = ReturnStatement {
    return_label      :: Maybe Label
  , return_expression :: Maybe Expression
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 8.13 Null statement
{-
    null_statement ::=
      [ label : ] NULL ;
-}

data NullStatement = NullStatement {
    null_label :: Maybe Label
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--
--                                   -- 9 --
--
--                            Concurrent statements
--
--------------------------------------------------------------------------------

{-
    concurrent_statement ::=
        block_statement
      | process_statement
      | concurrent_procedure_call_statement
      | concurrent_assertion_statement
      | concurrent_signal_assignment_statement
      | component_instantiation_statement
      | generate_statement
-}

data ConcurrentStatement =
    ConBlock     BlockStatement
  | ConProcess   ProcessStatement
  | ConProcCall  ConcurrentProcedureCallStatement
  | ConAssertion ConcurrentAssertionStatement
  | ConSignalAss ConcurrentSignalAssignmentStatement
  | ConComponent ComponentInstantiationStatement
  | ConGenerate  GenerateStatement
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 9.1 Block statement
{-
    block_statement ::=
      block_label :
        BLOCK [ ( guard_expression ) ] [ IS ]
          block_header
          block_declarative_part
        BEGIN
          block_statement_part
        END BLOCK [ block_label ] ;

    block_header ::=
      [ generic_clause
        [ generic_map_aspect ; ] ]
      [ port_clause
        [ port_map_aspect ; ] ]

    block_declarative_part ::=
      { block_declarative_item }

    block_statement_part ::=
      { concurrent_statement }
-}

data BlockStatement = BlockStatement {
    blocks_label            :: Label
  , blocks_guard_expression :: Maybe Expression
  , blocks_header           :: BlockHeader
  , blocks_declarative_part :: BlockDeclarativePart
  , blocks_statment_part    :: BlockStatementPart
  }
  deriving (Eq, Show)

data BlockHeader = BlockHeader {
    blockh_generic_clause   :: Maybe (GenericClause, Maybe GenericMapAspect)
  , blockh_port_clause      :: Maybe (PortClause,    Maybe PortMapAspect)
  }
  deriving (Eq, Show)

type BlockDeclarativePart = [BlockDeclarativeItem]

type BlockStatementPart   = [ConcurrentStatement]

--------------------------------------------------------------------------------
-- * 9.2 Process statement
{-
    process_statement ::=
      [ process_label : ]
        [ POSTPONED ] PROCESS [ ( sensitivity_list ) ] [ IS ]
          process_declarative_part
        BEGIN
          process_statement_part
        END [ POSTPONED ] PROCESS [ process_label ] ;

    process_declarative_part ::=
      { process_declarative_item }

    process_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | variable_declaration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | use_clause
      | group_type_declaration

    process_statement_part ::=
      { sequential_statement }
-}

data ProcessStatement = ProcessStatement {
    procs_label            :: Maybe Label
  , procs_postponed        :: Bool
  , procs_sensitivity_list :: Maybe SensitivityList
  , procs_declarative_part :: ProcessDeclarativePart
  , procs_statement_part   :: ProcessStatementPart
  }
  deriving (Eq, Show)

type ProcessDeclarativePart = [ProcessDeclarativeItem]

data ProcessDeclarativeItem =
    PDISubprogDecl SubprogramDeclaration
  | PDISubprogBody SubprogramBody
  | PDIType        TypeDeclaration
  | PDISubtype     SubtypeDeclaration
  | PDIConstant    ConstantDeclaration
  | PDIVariable    VariableDeclaration
  | PDIFile        FileDeclaration
  | PDIAlias       AliasDeclaration
  | PDIAttrDecl    AttributeDeclaration
  | PDIAttrSpec    AttributeSpecification
  | PDIUseClause   UseClause
--  | ProcDIGroupType   ()
  deriving (Eq, Show)

type ProcessStatementPart = [SequentialStatement]

--------------------------------------------------------------------------------
-- * 9.3 Concurrent procedure call statements
{-
    concurrent_procedure_call_statement ::=
      [ label : ] [ POSTPONED ] procedure_call ;
-}

data ConcurrentProcedureCallStatement = ConcurrentProcedureCallStatement {
    cpcs_label          :: Maybe Label
  , cpcs_postponed      :: Bool
  , cpcs_procedure_call :: ProcedureCall
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 9.4 Concurrent assertion statements
{-
    concurrent_assertion_statement ::=
      [ label : ] [ POSTPONED ] assertion ;
-}

data ConcurrentAssertionStatement = ConcurrentAssertionStatement {
    cas_label          :: Maybe Label
  , cas_postponed      :: Bool
  , cas_assertion      :: Assertion
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 9.5 Concurrent signal assignment statements
{-
    concurrent_signal_assignment_statement ::=
        [ label : ] [ POSTPONED ] conditional_signal_assignment
      | [ label : ] [ POSTPONED ] selected_signal_assignment

    options ::= [ GUARDED ] [ delay_mechanism ]
-}

data ConcurrentSignalAssignmentStatement =
    CSASCond {
      csas_cond_label               :: Maybe Label
    , csas_cond_postponed           :: Bool
    , csas_cond_signal_assignment   :: ConditionalSignalAssignment
    }
  | CSASSelect {
      csas_select_label             :: Maybe Label
    , csas_select_postponed         :: Bool
    , csas_select_signal_assignment :: SelectedSignalAssignment
    }
  deriving (Eq, Show)

data Options = Options {
    options_guarded         :: Bool
  , options_delay_mechanism :: Maybe DelayMechanism
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 9.5.1 Conditional signal assignments
{-
    conditional_signal_assignment ::=
      target <= options conditional_waveforms ;

    conditional_waveforms ::=
      { waveform WHEN condition ELSE }
      waveform [ WHEN condition ]
-}

data ConditionalSignalAssignment = ConditionalSignalAssignment {
    csa_target                :: Target
  , csa_options               :: Options
  , csa_conditional_waveforms :: ConditionalWaveforms
  }
  deriving (Eq, Show)

data ConditionalWaveforms = ConditionalWaveforms {
    cw_optional              :: [(Waveform, Condition)]
  , cw_wave                  :: (Waveform, Maybe Condition)
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 9.5.2 Selected signal assignments
{-
    selected_signal_assignment ::=
      WITH expression SELECT
        target <= options selected_waveforms ;

    selected_waveforms ::=
      { waveform WHEN choices , }
      waveform WHEN choices
-}

data SelectedSignalAssignment = SelectedSignalAssignment {
    ssa_expression         :: Expression
  , ssa_target             :: Target
  , ssa_options            :: Options
  , ssa_selected_waveforms :: SelectedWaveforms
  }
  deriving (Eq, Show)

data SelectedWaveforms = SelectedWaveforms {
    sw_optional :: Maybe [(Waveform, Choices)]
  , sw_last     :: (Waveform, Choices)
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * 9.6 Component instantiation statements
{-
    component_instantiation_statement ::=
      instantiation_label :
        instantiated_unit
          [ generic_map_aspect ]
          [ port_map_aspect ] ;

    instantiated_unit ::=
        [ COMPONENT ] component_name
      | ENTITY entity_name [ ( architecture_identifier ) ]
      | CONFIGURATION configuration_name
-}

data ComponentInstantiationStatement = ComponentInstantiationStatement {
    cis_instantiation_label :: Label
  , cis_instantiated_unit   :: InstantiatedUnit
  , cis_generic_map_aspect  :: Maybe GenericMapAspect
  , cis_port_map_aspect     :: Maybe PortMapAspect
  }
  deriving (Eq, Show)

data InstantiatedUnit =
    IUComponent Name
  | IUEntity    Name (Maybe Identifier)
  | IUConfig    Name
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 9.6.1 Instantiation of a component

--------------------------------------------------------------------------------
-- ** 9.6.2 Instantiation of a design entity

--------------------------------------------------------------------------------
-- * 9.7 Generate statements
{-
    generate_statement ::=
      generate_label :
        generation_scheme GENERATE
          [ { block_declarative_item }
        BEGIN ]
          { concurrent_statement }
        END GENERATE [ generate_label ] ;

    generation_scheme ::=
        FOR generate_parameter_specification
      | IF condition

    label ::= identifier
-}

data GenerateStatement = GenerateStatement {
    gens_label                  :: Label
  , gens_generation_scheme      :: GenerationScheme
  , gens_block_declarative_item :: Maybe (BlockDeclarativeItem)
  , gens_concurrent_statement   :: [ConcurrentStatement]
  }
  deriving (Eq, Show)

data GenerationScheme =
    GSFor ParameterSpecification
  | GSIf Condition
  deriving (Eq, Show)

type Label = Identifier
--------------------------------------------------------------------------------
--
--                                  -- 10 --
--
--                            Scope and visibility
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** 10.1 Declarative region

--------------------------------------------------------------------------------
-- ** 10.2 Scope of declarations

--------------------------------------------------------------------------------
-- ** 10.3 Visibility

--------------------------------------------------------------------------------
-- ** 10.4 Use clauses

{-
    use_clause ::=
      USE selected_name { , selected_name } ;
-}

data UseClause = UseClause [SelectedName]
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 10.5 The context of overload resolution

--------------------------------------------------------------------------------
--
--                                  -- 11 --
--
--                        Design units and their analysis
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** 11.1 Design units

{-
    design_file ::= design_unit { design_unit }

    design_unit ::= context_clause library_unit

    library_unit ::=
        primary_unit
      | secondary_unit

    primary_unit ::=
        entity_declaration
      | configuration_declaration
      | package_declaration

    secondary_unit ::=
        architecture_body
      | package_body
-}

data DesignFile = DesignFile [DesignUnit]
  deriving (Eq, Show)

data DesignUnit = DesignUnit ContextClause LibraryUnit
  deriving (Eq, Show)

data LibraryUnit =
    LibraryPrimary   PrimaryUnit
  | LibrarySecondary SecondaryUnit
  deriving (Eq, Show)

data PrimaryUnit =
    PrimaryEntity  EntityDeclaration
  | PrimaryConfig  ConfigurationDeclaration
  | PrimaryPackage PackageDeclaration
  deriving (Eq, Show)

data SecondaryUnit =
    SecondaryArchitecture ArchitectureBody
  | SecondaryPackage      PackageBody
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 11.2 Design libraries

{-
    library_clause ::= LIBRARY logical_name_list ;

    logical_name_list ::= logical_name { , logical_name }

    logical_name ::= identifier
-}

data LibraryClause = LibraryClause LogicalNameList
  deriving (Eq, Show)

data LogicalNameList = LogicalNameList [LogicalName]
  deriving (Eq, Show)

type LogicalName = Identifier

--------------------------------------------------------------------------------
-- ** 11.3 Context clauses

{-
    context_clause ::= { context_item }

    context_item ::=
        library_clause
      | use_clause
-}

data ContextClause = ContextClause [ContextItem]
  deriving (Eq, Show)

data ContextItem =
    ContextLibrary LibraryClause
  | ContextUse     UseClause
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- ** 11.3 Order of analysis

--------------------------------------------------------------------------------
--
--                                  -- 12 --
--
--                           Elaboration and execution
--
--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
--
--                                  -- 13 --
--
--                              Lexical elements
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** 13.4

{-
    abstract_literal ::= decimal_literal | based_literal
-}

data AbstractLiteral  =
      ALitDecimal DecimalLiteral
    | ALitBased   BasedLiteral
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 13.4.1
--
-- I use Haskell's Integer to represent integers in VHDL. Its syntax seems to be
-- slightly different though (the underline part).

{-
    decimal_literal ::= integer [ . integer ] [ exponent ]
    
    integer ::= digit { [ underline ] digit }

    exponent ::= E [ + ] integer | E – integer
-}

data DecimalLiteral = DecimalLiteral {
    decimal_integral_part   :: Integer
  , decimal_fractional_part :: Maybe Integer
  , decimal_exponent        :: Maybe Exponent
  }
  deriving (Eq, Show)

data Exponent =
    ExponentPos Integer
  | ExponentNeg Integer
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- *** 13.4.2

{-
    based_literal ::=
      base # based_integer [ . based_integer ] # [ exponent ]

    base ::= integer

    based_integer ::=
      extended_digit { [ underline ] extended_digit }

    extended_digit ::= digit | letter
-}

data BasedLiteral = BasedLiteral {
    based_lit_base                  :: Base
  , based_lit_based_integral_part   :: BasedInteger
  , based_lit_based_fractional_part :: Maybe BasedInteger
  , based_lit_exponent              :: Maybe Exponent
  }
  deriving (Eq, Show)

type Base = Integer

type BasedInteger = Integer

--------------------------------------------------------------------------------
--
--                                  - ToDo -
--
--------------------------------------------------------------------------------

data Identifier       = Ident String
  deriving (Eq, Show)

data CharacterLiteral = CLit Char
  deriving (Eq, Show)

data StringLiteral    = SLit String
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data BaseSpecifier =
    BSOctal
  | BSBinary
  | BSHexadecimal
  deriving (Eq, Show)

data BaseUnitDeclaration = BaseUnitDeclaration
  deriving (Eq, Show)

data BasicCharacter = BasicCharacter
  deriving (Eq, Show)

data BasicGraphicCharacter = BasicGraphicCharacter
  deriving (Eq, Show)

data BasicIdentifier = BasicIdentifier
  deriving (Eq, Show)

data BitStringLiteral = BitStringLiteral {
    bsl_base_specifier :: BaseSpecifier
  , bsl_bit_value :: BitValue
  }
  deriving (Eq, Show)

data BitValue = BitValue [ExtendedDigit]
  deriving (Eq, Show)

data ExtendedDigit = ExtendedDigit Char
  deriving (Eq, Show)

data ExtendedIdentifier = ExtendedIdentifier
  deriving (Eq, Show)

data GraphicCharacter = GraphicCharacter
  deriving (Eq, Show)

data Letter = Letter
  deriving (Eq, Show)

data LetterOrDigit = LetterOrDigit
  deriving (Eq, Show)

--------------------------------------------------------------------------------
