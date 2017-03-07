USE AcademyXI
GO

IF object_id(N'docenten.planning') IS NOT NULL
    DROP TABLE docenten.planning
GO

CREATE TABLE docenten.planning (
  Vluchtnr    [varchar](max) NULL,
  Airlinecode [varchar](max) NULL,
  Destcode    [varchar](max) NULL,
  Planterminal[char](1)      NULL,
  Plangate    [varchar](max) NULL,
  Plantijd    [varchar](max) NULL
)

IF object_id(N'docenten.vlucht') IS NOT NULL
    DROP TABLE docenten.vlucht
GO

CREATE TABLE docenten.vlucht (
  Vluchtid      [int]          NULL,
  Vluchtnr      [varchar](max) NULL,
  Airlinecode   [varchar](max) NULL,
  Destcode      [varchar](max) NULL,
  Vliegtuigcode [varchar](max) NULL
)

IF object_id(N'docenten.vertrek') IS NOT NULL
    DROP TABLE docenten.vertrek
GO

CREATE TABLE docenten.vertrek (
  Vluchtid      [int]          NULL,
  Vliegtuigcode [varchar](max) NULL,
  Terminal      [char](1)      NULL,
  Gate          [varchar](max) NULL,
  Baan          [int]          NULL,
  Bezetting     [int]          NULL,
  Vracht        [int]          NULL,
  Vertrektijd   [datetime]     NULL
)

IF object_id(N'docenten.aankomst') IS NOT NULL
    DROP TABLE docenten.aankomst
GO

CREATE TABLE docenten.aankomst (
  Vluchtid      [int]          NULL,
  Vliegtuigcode [varchar](max) NULL,
  Terminal      [char](1)      NULL,
  Gate          [varchar](max) NULL,
  Baan          [int]          NULL,
  Bezetting     [int]          NULL,
  Vracht        [int]          NULL,
  Aankomsttijd  [datetime]     NULL
)

IF object_id(N'docenten.klanttevredenheid') IS NOT NULL
    DROP TABLE docenten.klanttevredenheid
GO

CREATE TABLE docenten.klanttevredenheid (
  Vluchtid      [int]          NULL,
  Operatie      [float]        NULL,
  Faciliteiten  [float]        NULL,
  Shops         [float]        NULL
)

IF object_id(N'docenten.vliegtuig') IS NOT NULL
    DROP TABLE docenten.vliegtuig
GO

CREATE TABLE docenten.vliegtuig (
  Airlinecode   [varchar](max) NULL,
  Vliegtuigcode [varchar](max) NULL,
  Vliegtuigtype [varchar](max) NULL,
  Bouwjaar      [int]          NULL
)

IF object_id(N'docenten.vliegtuigtype') IS NOT NULL
    DROP TABLE docenten.vliegtuigtype
GO

CREATE TABLE docenten.vliegtuigtype (
  IATA          [varchar](3)   NULL,
  ICAO          [varchar](4)   NULL,
  Merk          [varchar](max) NULL,
  Type          [varchar](max) NULL,
  Wake          [char](1)      NULL,
  Cat           [varchar](max) NULL,
  Capaciteit    [int]          NULL,
  Vracht        [int]          NULL
)

IF object_id(N'docenten.maatschappij') IS NOT NULL
    DROP TABLE docenten.maatschappij
GO

CREATE TABLE docenten.maatschappij (
  Name  [varchar](max) NULL,
  IATA  [varchar](max) NULL,
  ICAO  [varchar](max) NULL
)

IF object_id(N'docenten.luchthavens') IS NOT NULL
    DROP TABLE docenten.luchthavens
GO

CREATE TABLE docenten.luchthavens (
  Airport     [varchar](max) NULL,
  City        [varchar](max) NULL,
  Country     [varchar](max) NULL,
  IATA        [char](3)      NULL,
  ICAO        [char](4)      NULL,
  Lat         [float]        NULL,
  Lon         [float]        NULL,
  Alt         [int]          NULL,
  TZ          [float]        NULL,
  DST         [char](1)      NULL,
  T_z         [varchar](max) NULL
)

IF object_id(N'docenten.baan') IS NOT NULL
    DROP TABLE docenten.baan
GO

CREATE TABLE docenten.baan (
  Baannummer  [int]          NULL,
  Code        [varchar](max) NULL,
  Naam        [varchar](max) NULL,
  Lengte      [char](4)      NULL
)