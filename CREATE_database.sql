USE AcademyXI
GO

IF object_id(N'docenten.Planning') IS NOT NULL
    DROP TABLE docenten.Planning
GO

CREATE TABLE docenten.Planning (
  Vluchtnr    [varchar](max) NULL,
  Airlinecode [varchar](max) NULL,
  Destcode    [varchar](max) NULL,
  Planterminal[char](1)      NULL,
  Plangate    [varchar](max) NULL,
  Plantijd    [varchar](max) NULL
)
GO

IF object_id(N'docenten.Vlucht') IS NOT NULL
    DROP TABLE docenten.Vlucht
GO

CREATE TABLE docenten.Vlucht (
  Vluchtid      [int]          NULL,
  Vluchtnr      [varchar](max) NULL,
  Airlinecode   [varchar](max) NULL,
  Destcode      [varchar](max) NULL,
  Vliegtuigcode [varchar](max) NULL
)
GO

IF object_id(N'docenten.VluchtVertrek') IS NOT NULL
    DROP TABLE docenten.VluchtVertrek
GO

CREATE TABLE docenten.VluchtVertrek (
  Vluchtid      [int]          NULL,
  Vliegtuigcode [varchar](max) NULL,
  Terminal      [char](1)      NULL,
  Gate          [varchar](max) NULL,
  Baan          [int]          NULL,
  Bezetting     [int]          NULL,
  Vracht        [int]          NULL,
  Vertrektijd   [datetime]     NULL
)
GO

IF object_id(N'docenten.VluchtAankomst') IS NOT NULL
    DROP TABLE docenten.VluchtAankomst
GO

CREATE TABLE docenten.VluchtAankomst (
  Vluchtid      [int]          NULL,
  Vliegtuigcode [varchar](max) NULL,
  Terminal      [char](1)      NULL,
  Gate          [varchar](max) NULL,
  Baan          [int]          NULL,
  Bezetting     [int]          NULL,
  Vracht        [int]          NULL,
  Aankomsttijd  [datetime]     NULL
)
GO

IF object_id(N'docenten.Klant') IS NOT NULL
    DROP TABLE docenten.Klant
GO

CREATE TABLE docenten.Klant (
  Vluchtid      [int]          NULL,
  Operatie      [float]        NULL,
  Faciliteiten  [float]        NULL,
  Shops         [float]        NULL
)
GO

IF object_id(N'docenten.Vliegtuig') IS NOT NULL
    DROP TABLE docenten.Vliegtuig
GO

CREATE TABLE docenten.Vliegtuig (
  Airlinecode   [varchar](max) NULL,
  Vliegtuigcode [varchar](max) NULL,
  Vliegtuigtype [varchar](max) NULL,
  Bouwjaar      [int]          NULL
)

IF object_id(N'docenten.Vliegtuigtype') IS NOT NULL
    DROP TABLE docenten.Vliegtuigtype
GO

CREATE TABLE docenten.Vliegtuigtype (
  IATA          [varchar](3)   NULL,
  ICAO          [varchar](4)   NULL,
  Merk          [varchar](max) NULL,
  Type          [varchar](max) NULL,
  Wake          [char](1)      NULL,
  Cat           [varchar](max) NULL,
  Capaciteit    [int]          NULL,
  Vracht        [int]          NULL
)
GO

IF object_id(N'docenten.Maatschappij') IS NOT NULL
    DROP TABLE docenten.Maatschappij
GO

CREATE TABLE docenten.Maatschappij (
  Name  [varchar](max) NULL,
  IATA  [varchar](max) NULL,
  ICAO  [varchar](max) NULL
)

IF object_id(N'docenten.Luchthaven') IS NOT NULL
    DROP TABLE docenten.Luchthaven
GO

CREATE TABLE docenten.Luchthaven (
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
GO

IF object_id(N'docenten.Baan') IS NOT NULL
    DROP TABLE docenten.Baan
GO

CREATE TABLE docenten.Baan (
  Baannummer  [int]          NULL,
  Code        [varchar](max) NULL,
  Naam        [varchar](max) NULL,
  Lengte      [char](4)      NULL
)
GO

IF object_id(N'docenten.WeerDeBiltHist') IS NOT NULL
    DROP TABLE docenten.WeerDeBiltHist
GO

CREATE TABLE docenten.WeerDeBiltHist (
    Datum DATE,
    DDVEC INT,
    FHVEC INT,
    FG    INT,
    FHX   INT,
    FHXH  INT,
    FHN   INT,
    FHNH  INT,
    FXX   INT,
    FXXH  INT,
    TG    INT,
    TN    INT,
    TNH   INT,
    TX    INT,
    TXH   INT,
    T10N  INT,
    T10NH INT,
    SQ    INT,
    SP    INT,
    Q     INT,
    DR    INT,
    RH    INT,
    RHX   INT,
    RHXH  INT,
    PG    INT,
    PX    INT,
    PXH   INT,
    PN    INT,
    PNH   INT,
    VVN   INT,
    VVNH  INT,
    VVX   INT,
    VVXH  INT,
    NG    INT,
    UG    INT,
    UX    INT,
    UXH   INT,
    UN    INT,
    UNH   INT,
    EV2   INT
)
GO

CREATE VIEW studenten.VluchtAankomst AS (
    SELECT
      *
    FROM docenten.vluchtAankomst a
    WHERE Aankomsttijd < GETDATE()
          AND Aankomsttijd >= CAST('2017-02-01' AS DATE) AND Aankomsttijd < CAST('2017-03-01' AS DATE)
)
GO

CREATE VIEW studenten.VluchtVertrek AS (
    SELECT
      *
    FROM docenten.vluchtVertrek
    WHERE Vertrektijd < GETDATE()
    AND Vertrektijd >= CAST('2017-02-01' AS DATE) AND Vertrektijd < CAST('2017-03-01' AS DATE)
)
GO

CREATE VIEW studenten.Vlucht AS (
    SELECT
      vl.*
    FROM docenten.vlucht vl
    LEFT JOIN docenten.vluchtAankomst aan
        ON vl.Vluchtid = aan.Vluchtid
    LEFT JOIN docenten.vluchtVertrek ver
        ON vl.Vluchtid = ver.Vluchtid
    WHERE vl.Datum <= CAST(GETDATE() AS DATE) AND ((aan.Aankomsttijd IS NULL AND ver.Vertrektijd IS NULL) OR aan.Aankomsttijd < GETDATE() OR ver.Vertrektijd < GETDATE())
          AND Datum >= CAST('2017-02-01' AS DATE) AND Datum < CAST('2017-03-01' AS DATE)
)
GO

CREATE VIEW studenten.Luchthaven AS (
    SELECT * FROM docenten.luchthaven
)
GO

CREATE VIEW studenten.Planning AS (
    SELECT * FROM docenten.planning
)
GO

CREATE VIEW studenten.Vliegtuig AS (
    SELECT * FROM docenten.vliegtuig
)
GO

CREATE VIEW studenten.Maatschappij AS(
    SELECT * FROM docenten.maatschappij
)
GO

CREATE VIEW studenten.WeerDeBiltHist AS (
    SELECT * FROM docenten.WeerDeBiltHist WHERE Datum < GETDATE()
)
GO