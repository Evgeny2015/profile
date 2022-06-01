unit Net.Svr;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Winapi.WinSock, Winapi.Windows,
  Net.Cnst, Net.Socket, Net.Utils, Net.Stream,
  Crypts;

const
  MAX_BINARY_SIZE                         = 4096;


  CI_ACCESS_APPLICATION_NAME_MAX_SIZE     = MAX_STRING_SIZE;
  CI_ACCESS_COMPUTER_NAME_MAX_SIZE        = MAX_STRING_SIZE;
  CI_ACCESS_USER_NAME_MAX_SIZE            = 64;

  // access
  CI_ACCESS_PROHIBIT                      = 0;
  CI_ACCESS_READ                          = 1;
  CI_ACCESS_WRITE                         = 2;

  // entity ids

type
//  EIOFileException = class(Exception);
  ECommandParser = class(Exception);

  TAes128CryptoProvider = class(TCryptoProvider)
  private const
    CIPHER_KEY: TAESCipherKey128 = ;
  public
    function Decrypt(AEncryptedStream, AStream: TStream): boolean; override;
    function Encrypt(AStream, AEncryptedStream: TStream): boolean; override;
  end;

  TNetCryptoProvider = class(TCryptoProvider)
  private
    FKey: TAESCipherKey128;
    FKeyset: TAESCipherKeyset128;
    procedure SetKey(const Value: TAESCipherKey128);
  protected
    property Keyset: TAESCipherKeyset128 read FKeyset;
  public
    constructor Create(const AKey: TAESCipherKey128);
    function Decrypt(AEncryptedStream, AStream: TStream): boolean; override;
    function Encrypt(AStream, AEncryptedStream: TStream): boolean; override;
    property Key: TAESCipherKey128 read FKey write SetKey;
  end;

  TFieldSet = class
  type
    TFieldType = Word;
  private
    FCapacity: integer;
    FCount: integer;
    FFields: array of TFieldType;
  protected
    procedure Grow;
  public
    function IndexOf(const AField: TFieldType): integer;
    function Exists(const AField: TFieldType): boolean;
    procedure Add(const AField: TFieldType);
    procedure Clear;
    property Count: integer read FCount;
  end;

  TNetItem = class
  private
    FFields: TFieldSet;
  protected
    function GetSize: integer; virtual;
    function FieldExists(const AField: TFieldSet.TFieldType): boolean;

    function GetStringSerializeLength(const AStr: string): integer;
    function IsIntValid(const AInt, AMinValue, AMaxValue: integer): boolean;
    function IsStringValid(const AStr: string; const AMaxLength: integer): boolean;

    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; virtual;
    procedure CreateFields;
    procedure SaveProperty(AFile: TIOBuffer); virtual;
    procedure WriteClass(AFile: TIOBuffer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; virtual;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; virtual;
    function LoadFromFile(AFile: TIOBuffer): boolean; virtual;
    function Valid: boolean; virtual;
    procedure AddFields(const AFields: array of TFieldSet.TFieldType);
    procedure SaveToBuffer(ABuffer: TIOBuffer); virtual;
    procedure SaveToFile(AFile: TIOBuffer);
    property Fields: TFieldSet read FFields;
    property Size: integer read GetSize;
  end;
  TNetItemClass = class of TNetItem;

  TNetValue<T> = class(TNetItem)
  private
    FValue: T;
  protected
    function GetSize: integer; override;
  public
    constructor CreateValue(const AValue: T); overload;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Assign(AObject: TNetValue<T>);
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Value: T read FValue write FValue;
  end;

  TBoolValue = class(TNetValue<Byte>)
  type
    TBoolType = (btUndefined = 0, btFalse, btTrue, btError);
  public
    procedure Clear;
    function GetBoolType: TBoolType;
    function GetBoolValue(const ADefault: boolean): boolean;
    function HasValue: boolean;
    function IsFalse: boolean;
    function Valid: boolean; override;
    procedure SetBoolValue(const AValue: boolean);
  end;

  TStringValue = class(TNetValue<string>)
  protected
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function LoadFromFile(AFile: TIOBuffer): boolean; override;
    function Valid: boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
  end;

  TStringIntValue = class(TNetItem)
  private
    FIntValue: integer;
    FStringValue: string;
  protected
    function GetSize: integer; override;
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function Valid: boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property IntValue: integer read FIntValue write FIntValue;
    property StringValue: string read FStringValue write FStringValue;
  end;

  TXMLValue = class(TStringValue)
  public
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
  end;

  TNetArray<T> = class(TNetItem)
  private
    FValue: TArray<T>;
    function GetLength: integer;
    function GetValue(Index: integer): T;
    procedure SetLength(const Value: integer);
    procedure SetValue(Index: integer; const Value: T);
  protected
    function GetSize: integer; override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Length: integer read GetLength write SetLength;
    property Value[Index: integer]: T read GetValue write SetValue;
  end;

  TNetList<T> = class(TNetItem)
  private
    FItems: TList<T>;
  protected
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Items: TList<T> read FItems;
  end;

  TNetObjectList<T: TNetItem, constructor> = class(TNetItem)
  private
    FItems: TObjectList<T>;
    function GetHasItems: boolean;
  protected
    function GetSize: integer; override;
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Add: T;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function LoadFromFile(AFile: TIOBuffer): boolean; override;
    function Valid: boolean; override;
    procedure Append(AItem: T);
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property HasItems: boolean read GetHasItems;
    property Items: TObjectList<T> read FItems;
  end;

  TNetStream = class(TNetItem)
  private
    FStream: TMemoryStream;
  protected
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Clear;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Stream: TMemoryStream read FStream;
  end;

  TNetMemory = class(TNetItem)
  private
    FIOMemory: TIOMemory;
  public
    constructor Create; override;
    destructor Destroy; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Memory: TIOMemory read FIOMemory;
  end;

  TNetSetItem = class(TNetItem)
  type
    TSetCommand = class(TNetValue<Integer>)
    const
      CMD_UPDATE = $0001;
    public
      function Valid: boolean; override;
    end;
  private
    FCommand: TSetCommand;
    FItem: TNetItem;
  public
    destructor Destroy; override;
    property Command: TSetCommand read FCommand;
    property Item: TNetItem read FItem;
  end;

  TNetItemList = class(TNetItem)
  type
    TNetObjectItem = TPair<boolean, TNetItem>;
  private
    FObjs: TList<TNetObjectItem>;
  protected
    function GetSize: integer; override;
    property Objs: TList<TNetObjectItem> read FObjs;
  public
    constructor Create; override;
    destructor Destroy; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function Put(AObj: TNetItem; AOwned: boolean = False): TNetItemList;
    procedure Clear;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
  end;

  TInterfaceInfo = class(TNetItem)
  private
    FAddress: TIpAddress;
    FFlag: Cardinal;
    FMask: TIpAddress;
  protected
    function GetSize: integer; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function GetInterFlag(AFlag: Cardinal): string;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function LoadFromFile(AFile: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Address: TIpAddress read FAddress write FAddress;
    property Flag: Cardinal read FFlag write FFlag;
    property Mask: TIpAddress read FMask write FMask;
  end;

  TInterfacesInfo = class(TNetItem)
  private
    FInterfaces: TNetObjectList<TInterfaceInfo>;
  protected
    function GetSize: integer; override;
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    procedure RequestInterfaces;
    property Interfaces: TNetObjectList<TInterfaceInfo> read FInterfaces;
  end;

  TConnectInfo = class(TNetItem)
  private
    FAccess: integer;
    FAccount: string;
    FAddress: TIpAddress;
    FApplication: string;
    FComputerName: string;
    FExtention: integer;
    FInterfaceInfo: TInterfacesInfo;
    FNetLibraryDate: TDateTime;
    FNetLibraryInternalVersion: Cardinal;
    FNetLibraryVersion: Cardinal;
    FPort: Word;
    FTimeout: integer;
    FUser: string;
  protected
    function GetSize: integer; override;
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function Valid: boolean; override;
    procedure SetAddress(const AAddr: TSockAddrIn);
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Access: integer read FAccess write FAccess;
    property Account: string read FAccount write FAccount;
    property Address: TIpAddress read FAddress;
    property Application: string read FApplication write FApplication;
    property ComputerName: string read FComputerName write FComputerName;
    property Extention: integer read FExtention write FExtention;
    property InterfaceInfo: TInterfacesInfo read FInterfaceInfo;
    property NetLibraryDate: TDateTime read FNetLibraryDate write FNetLibraryDate;
    property NetLibraryInternalVersion: Cardinal read FNetLibraryInternalVersion write FNetLibraryInternalVersion;
    property NetLibraryVersion: Cardinal read FNetLibraryVersion write FNetLibraryVersion;
    property Port: Word read FPort;
    property Timeout: integer read FTimeout write FTimeout;
    property User: string read FUser write FUser;
  end;

  TConnectAcceptInfo = class(TNetItem)
  private
    FSessionID: integer;
  protected
    function GetSize: integer; override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property SessionID: integer read FSessionID write FSessionID;
  end;

  TAcceptHash = class(TNetItem)
  private
    FPasswordBase: Cardinal;
    FReplyBase: Cardinal;
    FReplyInterval: Cardinal;
    FRequestBase: Cardinal;
    FRequestInterval: Cardinal;
  protected
    function GetSize: integer; override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Generate;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property PasswordBase: Cardinal read FPasswordBase;
    property ReplyBase: Cardinal read FReplyBase;
    property ReplyInterval: Cardinal read FReplyInterval;
    property RequestBase: Cardinal read FRequestBase;
    property RequestInterval: Cardinal read FRequestInterval;
  end;

  TAcceptKey = class(TNetItem)
  private
    FReplyKey: TAESCipherKey128;
    FRequestKey: TAESCipherKey128;
  protected
    function GetSize: integer; override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Generate;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property ReplyKey: TAESCipherKey128 read FReplyKey write FReplyKey;
    property RequestKey: TAESCipherKey128 read FRequestKey write FRequestKey;
  end;

  TAuthorizeInfo = class(TNetItem)
  private
    FBase: Cardinal;
    FHashCode: THashCode;
    FPassHash: TSHA512HashCode;
  protected
    function GetSize: integer; override;
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure GetHash(const APassHash: TSHA512HashCode; var AHashCode: THashCode);
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    function Equal(ANetItem: TNetItem): boolean; override;
    function HashValid(const APassHash: TSHA512HashCode): boolean;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Generate(const APassword: string);
    procedure SetHashCode;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Base: Cardinal read FBase write FBase;
    property HashCode: THashCode read FHashCode;
  end;

  TNetPacket = class(TNetItem)
  private
    FNetItem: TNetItem;
    FOwnObject: boolean;
  public
    destructor Destroy; override;
    procedure LinkObject(AObj: TNetItem);
    procedure Release; virtual;
    procedure SetObject(AObj: TNetItem);
    property NetItem: TNetItem read FNetItem;
  end;

  TPacketCommand = type Cardinal;
  TRequestPacket = class(TNetPacket)
  private
    FCommand: TPacketCommand;
  protected
    function GetSize: integer; override;
  public
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Command: TPacketCommand read FCommand write FCommand;
  end;

  TAcceptHashPacket = class(TRequestPacket)
  private
    FConnectAccept: TAcceptHash;
  public
    constructor Create; override;
    property ConnectAccept: TAcceptHash read FConnectAccept;
  end;

  TClearHashPacket  = class(TRequestPacket)
  public
    constructor Create; override;
  end;

  TAcceptKeyPacket = class(TRequestPacket)
  private
    FAcceptKey: TAcceptKey;
  public
    constructor Create; override;
    property AcceptKey: TAcceptKey read FAcceptKey;
  end;

  TReplyPacket = class(TNetPacket)
  private
    FExtendedCount: integer;
    FExtended: TObjectList<TRequestPacket>;
    FStatus: Cardinal;
  protected
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    procedure Release; override;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property ExtendedCount: integer read FExtendedCount;
    property Extended: TObjectList<TRequestPacket> read FExtended;
    property Status: Cardinal read FStatus write FStatus;
  end;

  TNetBuffer<PacketT: TNetPacket, constructor; LocalT: class, constructor> = class;
  TNetHash = class
  type
    THashBuffer = array[0..15] of Cardinal;
  private
  const
    Padding: array[0..63] of Byte =
      ($80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      );
    R: array[0..63] of integer =
      (7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
      5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
      4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
      6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21);
    K: array[0..63] of Cardinal =
     (3614090360, 3905402710, 606105819, 3250441966, 4118548399, 1200080426, 2821735955, 4249261313,
     1770035416, 2336552879, 4294925233, 2304563134, 1804603682, 4254626195, 2792965006, 1236535329,
     4129170786, 3225465664, 643717713, 3921069994, 3593408605, 38016083, 3634488961, 3889429448,
     568446438, 3275163606, 4107603335, 1163531501, 2850285829, 4243563512, 1735328473, 2368359562,
     4294588738, 2272392833, 1839030562, 4259657740, 2763975236, 1272893353, 4139469664, 3200236656,
     681279174, 3936430074, 3572445317, 76029189, 3654602809, 3873151461, 530742520, 3299628645,
     4096336452, 1126891415, 2878612391, 4237533241, 1700485571, 2399980690, 4293915773, 2240044497,
     1873313359, 4264355552, 2734768916, 1309151649, 4149444226, 3174756917, 718787259, 3951481745);
  private
    FBase: Cardinal;
    FInterval: Cardinal;
    function LeftRotate32(X: Cardinal; Y: integer): Cardinal;
    procedure MD5Hash(var Hash: THashCode; const Buffer: THashBuffer);
  public
    constructor Create;
    function Valid(Buffer: Pointer; BufferSize: integer; const Hash: THashCode): boolean;
    procedure GetHash(Buffer: Pointer; BufferSize: integer; out Hash: THashCode);
    procedure NextBase;
    property Base: Cardinal read FBase write FBase;
    property Interval: Cardinal read FInterval write FInterval;
  end;

  TNetChanger<LocalT: class, constructor> = class;
  TNetBuffer<PacketT: TNetPacket, constructor; LocalT: class, constructor> = class
  private
    FBuffer: TIOMemory;
    FCryptoProvider: TCryptoProvider;
    FEncrypted: boolean;
    FHashed: boolean;
    FNetChanger: TNetChanger<LocalT>;
    FNetHash: TNetHash;
    FNetPacket: PacketT;
    procedure SetHashed(const Value: boolean);
  protected
    function NetBufferSize: integer; virtual;
    function CheckConnection: boolean;
    property CryptoProvider: TCryptoProvider read FCryptoProvider;
  public
    constructor Create(ANetChanger: TNetChanger<LocalT>);
    destructor Destroy; override;
    function Receive: boolean;
    function Send: boolean;
    procedure SetCryptoProvider(ACryptoProvider: TCryptoProvider);
    property Buffer: TIOMemory read FBuffer;
    property Encrypted: boolean read FEncrypted;
    property Hashed: boolean read FHashed write SetHashed;
    property NetChanger: TNetChanger<LocalT> read FNetChanger;
    property NetHash: TNetHash read FNetHash;
    property NetPacket: PacketT read FNetPacket;
  end;

  TRequestBuffer<LocalT: class, constructor> = class(TNetBuffer<TRequestPacket, LocalT>)
  public
    function NetBufferSize: integer; override;
  end;

  TReplyBuffer<LocalT: class, constructor> = class(TNetBuffer<TReplyPacket, LocalT>)
  public
    function NetBufferSize: integer; override;
  end;

  TOnNetChangerError = procedure (Sender: TObject; ANetChangerError: Integer; AErrObj: TObject) of object;
  TNetChanger<LocalT: class, constructor> = class
  private
    FMaxReceiveBufferSize: integer;
    FMaxSendBufferSize: integer;
    FRequestBuffer: TRequestBuffer<LocalT>;
    FReplyBuffer: TReplyBuffer<LocalT>;
    FTcpClient: TTcpClient<LocalT>;
    FTcpServer: TTcpServer<LocalT>;
    FOnNetChangerError: TOnNetChangerError;
  protected
    procedure NotifyError(ANetChangerError: Integer; AErrObj: TObject);
  public
    constructor Create(ATcpServer: TTcpServer<LocalT>; ATcpClient: TTcpClient<LocalT>);
    destructor Destroy; override;
    function Realize: boolean; virtual; abstract;
    function Receive(var Buffer; Size: integer): boolean;
    function Send(var Buffer; Size: integer): boolean;
    procedure HandleBuffer;
    property MaxSendBufferSize: integer read FMaxSendBufferSize;
    property RequestBuffer: TRequestBuffer<LocalT> read FRequestBuffer;
    property ReplyBuffer: TReplyBuffer<LocalT> read FReplyBuffer;
    property TcpClient: TTcpClient<LocalT> read FTcpClient;
    property TcpServer: TTcpServer<LocalT> read FTcpServer;
    property OnNetChangerError: TOnNetChangerError read FOnNetChangerError write FOnNetChangerError;
  end;

  TCommandParser<GlobalT; LocalT: class, constructor> = class;
  TCommandServer<GlobalT; LocalT: class, constructor> = class;
  TServerCmd<GlobalT; LocalT: class, constructor> = class
  private
    FCommand: TPacketCommand;
    FServerParser: TCommandParser<GlobalT, LocalT>;
    FUsePostExecution: boolean;
  protected
    procedure Execute(ACommandServer: TCommandServer<GlobalT, LocalT>); virtual;
    procedure PostExecute(ACommandServer: TCommandServer<GlobalT, LocalT>); virtual;
  public
    constructor Create(ACommand: TPacketCommand; AServerParser: TCommandParser<GlobalT, LocalT>);
    property Command: TPacketCommand read FCommand;
    property UsePostExecution: boolean read FUsePostExecution write FUsePostExecution;
    property ServerParser: TCommandParser<GlobalT, LocalT> read FServerParser;
  end;

  TCommandParser<GlobalT; LocalT: class, constructor> = class
  private
    FCommandServer: TCommandServer<GlobalT, LocalT>;
    FItems: TObjectList<TServerCmd<GlobalT, LocalT>>;
    function GetCount: integer;
  public
    constructor Create(ACommandServer: TCommandServer<GlobalT, LocalT>);
    destructor Destroy; override;
    function Add(ACmd: TServerCmd<GlobalT, LocalT>): integer;
    function GetServerHandler(const ACmd: TPacketCommand): TServerCmd<GlobalT, LocalT>;
    function IndexOf(const ACmd: TPacketCommand): integer;
    procedure Clear;
    procedure Remove(ACmd: TPacketCommand);
    property CommandServer: TCommandServer<GlobalT, LocalT> read FCommandServer;
    property Count: integer read GetCount;
  end;

  TCommandServer<GlobalT; LocalT: class, constructor> = class(TNetChanger<LocalT>)
  private
    FGlobalData: GlobalT;
    FParser: TCommandParser<GlobalT, LocalT>;
  public
    constructor Create(AGlobalData: GlobalT; ATcpServer: TTcpServer<LocalT>; ATcpClient: TTcpClient<LocalT>); virtual;
    destructor Destroy; override;
    property Global: GlobalT read FGlobalData;
    property Parser: TCommandParser<GlobalT, LocalT> read FParser;
  end;

  TNetServer<GlobalT; LocalT: class, constructor> = class(TCommandServer<GlobalT, LocalT>)
  public
    function Realize: boolean; override;
  end;

  TNetClient<GlobalT; LocalT: class, constructor> = class(TCommandServer<GlobalT, LocalT>)
  public
    function Realize: boolean; override;
  end;

//  function GetNodePath(ANode: IXMLNode): string;
  function NetLibraryVersionToString(Version: Cardinal): string;

implementation

uses
  System.Variants,
  Net.Store;

const
  DEFAULT_BUFFER_SIZE           = 16;

//function GetNodePath(ANode: IXMLNode): string;
//begin
//  Result := '';
//  while ANode <> nil do
//  begin
//    Result := '/' + ANode.NodeName + Result;
//    ANode := ANode.ParentNode;
//  end;
//end;

function NetLibraryVersionToString(Version: Cardinal): string;
begin
  Result :=
    IntToStr((Version shr 4) and $0f) + '.' +
    IntToStr(Version and $0f) + '.' +
    IntToStr((Version shr 8) and $ff) + '.' +
    IntToStr(Version shr 16);
end;


{ TAes128CryptoProvider }

function TAes128CryptoProvider.Encrypt(AStream, AEncryptedStream: TStream): boolean;
begin
  AES128EncryptMD5(AStream, AEncryptedStream, CIPHER_KEY);
  Result := True;
end;

function TAes128CryptoProvider.Decrypt(AEncryptedStream, AStream: TStream): boolean;
begin
  Result := AES128DecryptMD5(AStream, AEncryptedStream, CIPHER_KEY);
end;

{ TNetCryptoProvider }

constructor TNetCryptoProvider.Create(const AKey: TAESCipherKey128);
begin
  SetKey(AKey);
end;

function TNetCryptoProvider.Decrypt(AEncryptedStream, AStream: TStream): boolean;
begin
  Result := AES128Decrypt(AStream, AEncryptedStream, Keyset);
end;


function TNetCryptoProvider.Encrypt(AStream, AEncryptedStream: TStream): boolean;
begin
  AES128Encrypt(AStream, AEncryptedStream, Keyset);
  Result := True;
end;

procedure TNetCryptoProvider.SetKey(const Value: TAESCipherKey128);
begin
  FKey := Value;
  expand_key(Key, FKeyset);
end;

{ TRequestPacket }

function TRequestPacket.GetSize: integer;
begin
  Result := SizeOf(FCommand);
  if NetItem <> nil then
    Result := Result + NetItem.Size;
end;

function TRequestPacket.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  if not ABuffer.ContainData(SizeOf(TPacketCommand)) then
    Exit(False);

  ABuffer.Read(FCommand, SizeOf(TPacketCommand));
  Result := True;
end;

procedure TRequestPacket.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.Write(Command, SizeOf(TPacketCommand));
  if NetItem <> nil then
    NetItem.SaveToBuffer(ABuffer);
end;

{ TAcceptHashPacket }

constructor TAcceptHashPacket.Create;
begin
  inherited;
  FConnectAccept := TAcceptHash.Create;
  FConnectAccept.Generate;
  SetObject(FConnectAccept);
  Command := NC_EXTENDED_HASH_ENABLE_AND_SET_KEY;
end;

{ TClearHashPacket }

constructor TClearHashPacket.Create;
begin
  inherited;
  Command := NC_EXTENDED_HASH_CLEAR;
end;

{ TAcceptKeyPacket }

constructor TAcceptKeyPacket.Create;
begin
  inherited;
  FAcceptKey := TAcceptKey.Create;
  FAcceptKey.Generate;
  SetObject(FAcceptKey);
  Command := NC_EXTENDED_SET_CYPHER_KEY;
end;

{ TNetPacket }

destructor TNetPacket.Destroy;
begin
  Release;
  inherited;
end;

procedure TNetPacket.LinkObject(AObj: TNetItem);
begin
  {$IFDEF DEBUG}
    if FNetItem <> nil then
      raise Exception.Create('TNetPacket.SetNetObject');
  {$ENDIF}

  FOwnObject := False;
  FNetItem := AObj;
end;

procedure TNetPacket.Release;
begin
  if FOwnObject then
    FNetItem.Free;
  FNetItem := nil;
end;

procedure TNetPacket.SetObject(AObj: TNetItem);
begin
  {$IFDEF DEBUG}
    if FNetItem <> nil then
      raise Exception.Create('TNetPacket.SetNetObject');
  {$ENDIF}

  FOwnObject := True;
  FNetItem := AObj;
end;

{ TReplyPacket }

constructor TReplyPacket.Create;
begin
  inherited;
  FExtended := TObjectList<TRequestPacket>.Create;
end;

destructor TReplyPacket.Destroy;
begin
  FExtended.Free;
  inherited;
end;

function TReplyPacket.GetSize: integer;
var
  RequestPacket: TRequestPacket;
begin
  Result := SizeOf(FStatus) + SizeOf(FExtended.Count);

  if NetItem <> nil then
    Result := Result + NetItem.Size;

  for RequestPacket in Extended do
    Result := Result + RequestPacket.Size;
end;

function TReplyPacket.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  if not ABuffer.ContainData(SizeOf(ExtendedCount) + SizeOf(Cardinal)) then
    Exit(False);

  ABuffer.Read(FExtendedCount, SizeOf(FExtendedCount));
  ABuffer.Read(FStatus, SizeOf(Cardinal));
  Result := True;
end;

procedure TReplyPacket.Release;
begin
  inherited;
  Extended.Clear;
end;

procedure TReplyPacket.SaveToBuffer(ABuffer: TIOBuffer);
var
  RequestPacket: TRequestPacket;
begin
  ABuffer.Write(Extended.Count, SizeOf(Integer));
  ABuffer.Write(Status, SizeOf(Cardinal));

  if NetItem <> nil then
    NetItem.SaveToBuffer(ABuffer);

  for RequestPacket in Extended do
    RequestPacket.SaveToBuffer(ABuffer);
end;

{ TNetHash }

constructor TNetHash.Create;
begin
  FInterval := 1;
end;

procedure TNetHash.GetHash(Buffer: Pointer; BufferSize: integer; out Hash: THashCode);
var
  Data: THashBuffer;
  I: integer;
  Count: integer;
  Ptr: Pointer;
begin
  Hash[0] := $67452301;
  Hash[1] := $EFCDAB89;
  Hash[2] := $98BADCFE;
  Hash[3] := $10325476;

  Ptr := Buffer;
  Count := BufferSize shr 6;

  for I := 0 to Count-1 do
  begin
    Move(Ptr^, Data, SizeOf(Data));
    NativeInt(Ptr) := NativeInt(Ptr) + SizeOf(Data);
    MD5Hash(Hash, Data);
  end;

  I := BufferSize and $3f;
  Move(Ptr^, Data, I);

  if I < 56 then
  begin
    Move(Padding, PByteArray(@Data)^[I], 56-I);
    Move(Base, PByteArray(@Data)^[56], SizeOf(Base));
    Move(BufferSize, PByteArray(@Data)^[60], SizeOf(BufferSize));
    MD5Hash(Hash, Data);
  end
  else
  begin
    Move(Padding, PByteArray(@Data)^[I], 64-I);
    MD5Hash(Hash, Data);
    FillChar(Data, 56, 0);
    Move(Base, PByteArray(@Data)^[56], SizeOf(Base));
    Move(BufferSize, PByteArray(@Data)^[60], SizeOf(BufferSize));
    MD5Hash(Hash, Data);
  end;
end;

function TNetHash.LeftRotate32(X: Cardinal; Y: integer): Cardinal;
begin
  Result := (X shl Y) or (X shr (32-Y));
end;

procedure TNetHash.MD5Hash(var Hash: THashCode; const Buffer: THashBuffer);
var
  I: integer;
  A, B, C, D: Cardinal;
  F, G: Cardinal;
  tmp: integer;
begin
  A := Hash[0]; B := Hash[1]; C := Hash[2]; D := Hash[3];

  for I := 0 to 63 do
  begin
    if I <= 15 then
    begin
      F := D xor (B and (C xor D));
      G := I;
    end
    else if I <= 31 then
    begin
      F := C xor (D and (B xor C));
      G := (5*I + 1) mod 16;
    end
    else if I <= 47 then
    begin
      F := B xor C xor D;
      G := (3*I + 5) mod 16;
    end
    else
    begin
      F := C xor (B or (not D));
      G := (7*I) mod 16;
    end;

    tmp := D;
    D := C;
    C := B;
    B := B + LeftRotate32(A + F + K[I] + Buffer[G], R[I]);
    A := tmp;
  end;

  Hash[0] := Hash[0] + A;
  Hash[1] := Hash[1] + B;
  Hash[2] := Hash[2] + C;
  Hash[3] := Hash[3] + D;
end;

procedure TNetHash.NextBase;
begin
  FBase := FBase + FInterval;
end;

function TNetHash.Valid(Buffer: Pointer; BufferSize: integer; const Hash: THashCode): boolean;
var
  H: THashCode;
begin
  GetHash(Buffer, BufferSize, H);
  Result := (H[0] = Hash[0]) and (H[1] = Hash[1]) and (H[2] = Hash[2]) and (H[3] = Hash[3]);
end;

{ TFieldSet }

procedure TFieldSet.Add(const AField: TFieldType);
begin
  if Count = FCapacity then
    Grow;

  FFields[Count] := AField;
  Inc(FCount);
end;

procedure TFieldSet.Clear;
begin
  FCount := 0;
end;

procedure TFieldSet.Grow;
begin
  Inc(FCapacity, 4);
  SetLength(FFields, FCapacity);
end;

function TFieldSet.IndexOf(const AField: TFieldType): integer;
var
  I: integer;
begin
  Result := -1;

  for I := 0 to Length(FFields) - 1 do
    if FFields[I] = AField then
      Exit(I);
end;

function TFieldSet.Exists(const AField: TFieldType): boolean;
begin
  Result := IndexOf(AField) >= 0;
end;

{ TNetItem }

procedure TNetItem.AddFields(const AFields: array of TFieldSet.TFieldType);
var
  F: TFieldSet.TFieldType;
begin
  CreateFields;

  for F in AFields do
    Fields.Add(F);
end;

constructor TNetItem.Create;
begin
end;

procedure TNetItem.CreateFields;
begin
  if FFields = nil then
    FFields := TFieldSet.Create;
end;

destructor TNetItem.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TNetItem.Equal(ANetItem: TNetItem): boolean;
begin
  Result := False;
end;

function TNetItem.FieldExists(const AField: TFieldSet.TFieldType): boolean;
begin
  if FFields = nil then
    Exit(False);

  Result := Fields.Exists(AField);
end;

function TNetItem.GetSize: integer;
begin
  Result := 0;
end;

function TNetItem.GetStringSerializeLength(const AStr: string): integer;
begin
  Result := SizeOf(Integer) + Length(AStr)*SizeOf(Char);
end;

function TNetItem.IsIntValid(const AInt, AMinValue, AMaxValue: integer): boolean;
begin
  Result := (AInt >= AMinValue) and (AInt <= AMaxValue);
end;

function TNetItem.IsStringValid(const AStr: string; const AMaxLength: integer): boolean;
var
  L: integer;
begin
  L := Length(AStr);
  Result := (L > 0) and (L <= AMaxLength);
end;

function TNetItem.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := ABuffer.ContainData(Size);
end;

function TNetItem.LoadFromFile(AFile: TIOBuffer): boolean;
var
  TagId, TypeId: integer;
begin
  TagId := AFile.ReadTag;

  // create fields set
  CreateFields;
  FFields.Clear;

  while TagId <> CCloseTag_Id do
  begin
    TypeId := AFile.ReadType;

    Fields.Add(TagId);
    LoadProperty(TagId, TypeId, AFile);

    TagId := AFile.ReadTag;
  end;

  Result := True;
end;

function TNetItem.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := False;
end;

procedure TNetItem.SaveProperty(AFile: TIOBuffer);
begin
end;

procedure TNetItem.SaveToBuffer(ABuffer: TIOBuffer);
begin
end;

procedure TNetItem.SaveToFile(AFile: TIOBuffer);
begin
  WriteClass(AFile);
  SaveProperty(AFile);
  AFile.WriteTag(CCloseTag_Id);
end;

function TNetItem.Valid: boolean;
begin
  Result := True;
end;

procedure TNetItem.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTNetItem_Id);
end;

{ TNetValue<T> }

procedure TNetValue<T>.Assign(AObject: TNetValue<T>);
begin
  FValue := AObject.FValue;
end;

constructor TNetValue<T>.CreateValue(const AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

function TNetValue<T>.Equal(ANetItem: TNetItem): boolean;
begin
  Result := System.SysUtils.CompareMem(@FValue, @TNetValue<T>(ANetItem).FValue, SizeOf(T));
end;

function TNetValue<T>.GetSize: integer;
begin
  Result := SizeOf(T);
end;

function TNetValue<T>.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := ABuffer.ContainData(Size);
  if not Result then Exit;

  try
    ABuffer.Read(FValue, SizeOf(T));
  except
    Result := False;
  end;
end;

procedure TNetValue<T>.SaveToBuffer(ABuffer: TIOBuffer);
begin
  inherited;
  ABuffer.Write(Value, SizeOf(T));
end;

{ TBoolValue }

procedure TBoolValue.Clear;
begin
  Value := Byte(btUndefined);
end;

function TBoolValue.GetBoolType: TBoolType;
begin
  if Value >= Byte(btError) then
    Exit(btError);

  Result := TBoolType(Value);
end;

function TBoolValue.GetBoolValue(const ADefault: boolean): boolean;
begin
  case GetBoolType of
    btFalse:
      Exit(False);

    btTrue:
      Exit(True);

  else
    Result := ADefault;
  end;
end;

function TBoolValue.HasValue: boolean;
begin
  Result := GetBoolType in [btFalse, btTrue];
end;

function TBoolValue.IsFalse: boolean;
begin
  Result := Value = Byte(btFalse);
end;

procedure TBoolValue.SetBoolValue(const AValue: boolean);
begin
  if AValue then
    Value := Byte(btTrue)
  else
    Value := Byte(btFalse);
end;

function TBoolValue.Valid: boolean;
begin
  Result := GetBoolType in [btUndefined..btTrue];
end;

{ TStringValue }

function TStringValue.Equal(ANetItem: TNetItem): boolean;
begin
  Result := False;
  if not (ANetItem is TStringValue) then
    Exit;

  Result := Value = TStringValue(ANetItem).Value;
end;

function TStringValue.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := ABuffer.ContainData(SizeOf(Integer));
  if Result then
    Value := ABuffer.ReadString;
end;

function TStringValue.LoadFromFile(AFile: TIOBuffer): boolean;
begin
  LoadProperty(CUnknownTag_Id, CUnknownType_Id, AFile);
  AFile.ReadTag;
  Result := True;
end;

function TStringValue.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Value := AFile.ReadString;
  Result := True;
end;

procedure TStringValue.SaveProperty(AFile: TIOBuffer);
begin
  AFile.WriteString(Value);
end;

procedure TStringValue.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteString(Value);
end;

function TStringValue.Valid: boolean;
begin
  Result := IsStringValid(Value, MAX_STRING_SIZE);
end;

procedure TStringValue.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTStringValue_Id);
end;

{ TStringIntValue }

function TStringIntValue.Equal(ANetItem: TNetItem): boolean;
var
  Src: TStringIntValue;
begin
  Result := False;
  if not (ANetItem is TStringIntValue) then
    Exit;
  Src := TStringIntValue(ANetItem);

  Result := (IntValue = Src.IntValue) and (StringValue = Src.StringValue);
end;

function TStringIntValue.GetSize: integer;
begin
  Result := SizeOf(IntValue) + GetStringSerializeLength(StringValue);
end;

function TStringIntValue.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
//  Result := ABuffer.ContainData(SizeOf(IntValue));
  IntValue := ABuffer.ReadInteger;

//  Result := ABuffer.ContainData(SizeOf(Integer));
  StringValue := ABuffer.ReadString;
  Result := True;
end;

function TStringIntValue.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := True;

  case ATagId of
    CPStringIntValue_IntValue_Id:
      IntValue := AFile.ReadInteger;

    CPStringIntValue_StrValue_Id:
      StringValue := AFile.ReadString;

  else
    Result := inherited LoadProperty(ATagId, ATypeId, AFile);
  end;
end;

procedure TStringIntValue.SaveProperty(AFile: TIOBuffer);
begin
  AFile.WriteProp(CPStringIntValue_IntValue_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(IntValue);

  AFile.WriteProp(CPStringIntValue_StrValue_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(StringValue);
end;

procedure TStringIntValue.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteInteger(IntValue);
  ABuffer.WriteString(StringValue);
end;

function TStringIntValue.Valid: boolean;
begin
  Result := IsStringValid(StringValue, MAX_STRING_SIZE);
end;

procedure TStringIntValue.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTStringIntValue_Id);
end;

{ TXMLValue }

function TXMLValue.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := ABuffer.ContainData(SizeOf(Integer));
  if Result then
    Value := ABuffer.ReadString;
end;


procedure TXMLValue.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteString(Value, MAX_XML_SIZE);
end;

{ TNetArray<T> }

function TNetArray<T>.Equal(ANetItem: TNetItem): boolean;
var
  Src: TNetArray<T>;
begin
  Result := False;
  if not (ANetItem is TNetArray<T>) then
    Exit;
  Src := TNetArray<T>(ANetItem);

  if Length <> Src.Length then Exit;

  Result := System.SysUtils.CompareMem(@FValue[0], @Src.FValue[0], Length*SizeOf(T));
end;

function TNetArray<T>.GetLength: integer;
begin
  Result := System.Length(FValue);
end;

function TNetArray<T>.GetSize: integer;
begin
  Result := SizeOf(Integer) + SizeOf(T)*Length;
end;

function TNetArray<T>.GetValue(Index: integer): T;
begin
  Result := FValue[Index];
end;

function TNetArray<T>.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  I, L: integer;
begin
  if not ABuffer.ContainData(SizeOf(Integer))
    then Exit(False);

  L := ABuffer.ReadInteger;
  if (L < 0) or not ABuffer.ContainData(L*SizeOf(T))
    then Exit(False);

  Length := L;
  if L = 0 then Exit(True);

  ABuffer.Read(FValue[0], L*SizeOf(T));
  Result := True;
end;

procedure TNetArray<T>.SaveToBuffer(ABuffer: TIOBuffer);
var
  L: integer;
begin
  L := Length;
  ABuffer.WriteInteger(L);
  if L = 0 then Exit;

  ABuffer.Write(FValue[0], L*SizeOf(T));
end;

procedure TNetArray<T>.SetLength(const Value: integer);
begin
  System.SetLength(FValue, Value);
end;

procedure TNetArray<T>.SetValue(Index: integer; const Value: T);
begin
  FValue[Index] := Value;
end;

{ TNetList<T> }

constructor TNetList<T>.Create;
begin
  inherited;
  FItems := TList<T>.Create;
end;

destructor TNetList<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TNetList<T>.Equal(ANetItem: TNetItem): boolean;
var
  Src: TNetList<T>;
begin
  Result := False;
  if not (ANetItem is TNetList<T>) then
    Exit;
  Src := TNetList<T>(ANetItem);

  if Items.Count <> Src.Items.Count then Exit;

  Result := System.SysUtils.CompareMem(@Items.List[0], @Src.Items.List[0], Items.Count*SizeOf(T));
end;

function TNetList<T>.GetSize: integer;
begin
  Result := SizeOf(Integer) + SizeOf(T)*Items.Count;
end;

function TNetList<T>.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  I, L: integer;
  V: T;
begin
  Items.Clear;

  if not ABuffer.ContainData(SizeOf(Integer))
    then Exit(False);

  L := ABuffer.ReadInteger;
  if (L < 0) or not ABuffer.ContainData(L*SizeOf(T))
    then Exit(False);

  if L = 0 then Exit(True);

  for I := 0 to L-1 do
  begin
    ABuffer.Read(V, SizeOf(T));
    Items.Add(V);
  end;

  Result := True;
end;

procedure TNetList<T>.SaveToBuffer(ABuffer: TIOBuffer);
var
  I, L: integer;
  V: T;
begin
  L := Items.Count;
  ABuffer.WriteInteger(L);
  if L = 0 then Exit;

  for I := 0 to L-1 do
  begin
    V := Items[I];
    ABuffer.Write(V, SizeOf(T));
  end;
end;

{ TNetObjectList<T> }

function TNetObjectList<T>.Add: T;
begin
  Result := T.Create;
  FItems.Add(Result);
end;

procedure TNetObjectList<T>.Append(AItem: T);
begin
  Items.Add(AItem);
end;

constructor TNetObjectList<T>.Create;
begin
  inherited;
  FItems := TObjectList<T>.Create;
end;

destructor TNetObjectList<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TNetObjectList<T>.Equal(ANetItem: TNetItem): boolean;
var
  Src: TNetObjectList<T>;
  I: integer;
begin
  Result := False;
  if not (ANetItem is TNetObjectList<T>) then
    Exit;
  Src := TNetObjectList<T>(ANetItem);

  if Items.Count <> Src.Items.Count then Exit;

  for I := 0 to Items.Count-1 do
    if not Items[I].Equal(Src.Items[I]) then
      Exit;

  Result := True;
end;

function TNetObjectList<T>.GetHasItems: boolean;
begin
  Result := Items.Count > 0;
end;

function TNetObjectList<T>.GetSize: integer;
var
  I: T;
begin
  Result := SizeOf(Integer);

  for I in Items do
    Result := Result + I.Size;
end;

function TNetObjectList<T>.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  I, L: integer;
  V: T;
begin
  Items.Clear;

  if not ABuffer.ContainData(SizeOf(Integer))
    then Exit(False);

  L := ABuffer.ReadInteger;
  if L < 0 then Exit(False);
  if L = 0 then Exit(True);

  for I := 0 to L-1 do
  begin
    V := Add;
    V.LoadFromBuffer(ABuffer);
  end;

  Result := True;
end;

function TNetObjectList<T>.LoadFromFile(AFile: TIOBuffer): boolean;
var
  Item: TNetItem;
begin
  Item := Net.Store.GetObject(AFile);

  while Item <> nil do
  begin
    Items.Add(Item);
    Item := Net.Store.GetObject(AFile);
  end;

  AFile.ReadTag;

  Result := True;
end;

function TNetObjectList<T>.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
var
  Item: TNetItem;
begin
  Item := Net.Store.GetObject(AFile);

  while Item <> nil do
  begin
    Items.Add(Item);
    Item := Net.Store.GetObject(AFile);
  end;
end;

procedure TNetObjectList<T>.SaveProperty(AFile: TIOBuffer);
var
  I: T;
begin
  AFile.WriteTag(COpenSet_Id);
  for I in Items do
    I.SaveToFile(AFile);
  AFile.WriteTag(CCloseSet_Id);
end;

procedure TNetObjectList<T>.SaveToBuffer(ABuffer: TIOBuffer);
var
  I, L: integer;
  V: T;
begin
  L := Items.Count;
  ABuffer.WriteInteger(L);
  if L = 0 then Exit;

  for I := 0 to L-1 do
  begin
    V := Items[I];
    V.SaveToBuffer(ABuffer);
  end;
end;

function TNetObjectList<T>.Valid: boolean;
var
  Item: T;
begin
  Result := inherited;

  for Item in Items do
    if not Item.Valid then
      Exit(False);
end;

procedure TNetObjectList<T>.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTNetObjectList_Id);
end;

{ TNetStream }

procedure TNetStream.Clear;
begin
  Stream.Size := 0;
end;

constructor TNetStream.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TNetStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TNetStream.Equal(ANetItem: TNetItem): boolean;
var
  Src: TNetStream;
begin
  Result := False;
  if not (ANetItem is TNetStream) then
    Exit;
  Src := TNetStream(ANetItem);

  if Stream.Size <> Src.Stream.Size then Exit;

  Result := System.SysUtils.CompareMem(Stream.Memory, Src.Stream.Memory, Stream.Size);
end;

function TNetStream.GetSize: integer;
begin
  Result := SizeOf(Integer) + FStream.Position;
end;

function TNetStream.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  L: integer;
begin
  if not ABuffer.ContainData(SizeOf(L)) then Exit(False);

  L := ABuffer.ReadInteger;
  if (L < 0) or not ABuffer.ContainData(L) then Exit(False);

  FStream.Size := L;
  FStream.Position := 0;

  ABuffer.Read(Stream.Memory^, L);
  Result := True;
end;

procedure TNetStream.SaveToBuffer(ABuffer: TIOBuffer);
var
  L: integer;
begin
  inherited;
  L := FStream.Position;
  ABuffer.WriteInteger(L);
  if L = 0 then Exit;

  ABuffer.Write(FStream.Memory^, L);
end;

{ TNetMemory }

constructor TNetMemory.Create;
begin
  inherited;
  FIOMemory := TIOMemory.Create;
end;

destructor TNetMemory.Destroy;
begin
  FIOMemory.Free;
  inherited;
end;


function TNetMemory.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  L: integer;
begin
  if not ABuffer.ContainData(SizeOf(L)) then Exit(False);

  L := ABuffer.ReadInteger;
  if (L < 0) or not ABuffer.ContainData(L) then Exit(False);

  Memory.Size := L;
  Memory.Seek();

  ABuffer.Read(Memory.Memory^, L);
  Result := True;
end;

procedure TNetMemory.SaveToBuffer(ABuffer: TIOBuffer);
var
  L: integer;
begin
  inherited;
  L := Memory.Position;
  ABuffer.WriteInteger(L);
  if L = 0 then Exit;

  ABuffer.Write(Memory.Memory^, L);
end;

{ TNetSetItem.TSetCommand }

function TNetSetItem.TSetCommand.Valid: boolean;
begin
  Result := Value = CMD_UPDATE;
end;

{ TNetSetItem }

destructor TNetSetItem.Destroy;
begin

  inherited;
end;

{ TNetItemList }

procedure TNetItemList.Clear;
var
  Obj: TNetObjectItem;
begin
  for Obj in Objs do
    if Obj.Key then
      Obj.Value.Free;

  Objs.Clear;
end;

constructor TNetItemList.Create;
begin
  inherited;
  FObjs := TList<TNetObjectItem>.Create;
end;

destructor TNetItemList.Destroy;
begin
  Clear;
  FObjs.Free;
  inherited;
end;

function TNetItemList.GetSize: integer;
var
  Obj: TNetObjectItem;
begin
  Result := 0;

  for Obj in Objs do
    Result := Result + Obj.Value.Size;
end;

function TNetItemList.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
var
  Obj: TNetObjectItem;
begin
  for Obj in Objs do
    if not Obj.Value.LoadFromBuffer(ABuffer) then
      Exit(False);

  Result := True;
end;

function TNetItemList.Put(AObj: TNetItem; AOwned: boolean): TNetItemList;
begin
  Objs.Add(TNetObjectItem.Create(AOwned, AObj));
  Result := self;
end;

procedure TNetItemList.SaveToBuffer(ABuffer: TIOBuffer);
var
  Obj: TNetObjectItem;
begin
  for Obj in Objs do
    Obj.Value.SaveToBuffer(ABuffer);
end;

{ TInterfaceInfo }

function TInterfaceInfo.Equal(ANetItem: TNetItem): boolean;
var
  Src: TInterfaceInfo;
begin
  Result := False;
  if not (ANetItem is TInterfaceInfo) then
    Exit;
  Src := TInterfaceInfo(ANetItem);

  Result := Address.Equal(Src.Address) and
    (Flag = Src.Flag) and
    Mask.Equal(Src.Mask);
end;

function TInterfaceInfo.GetInterFlag(AFlag: Cardinal): string;
var
  Strs: TStrings;
begin
  Strs := TStringList.Create;

  try
    Strs.Delimiter := ',';

    if AFlag and IFF_UP <> 0 then
      Strs.Add('UP');
    if AFlag and IFF_BROADCAST <> 0 then
      Strs.Add('BROADCAST');
    if AFlag and IFF_LOOPBACK <> 0 then
      Strs.Add('LOOPBACK');
    if AFlag and IFF_POINTTOPOINT <> 0 then
      Strs.Add('PTP');
    if AFlag and IFF_MULTICAST <> 0 then
      Strs.Add('MULTICAST');

    Result := Strs.DelimitedText;
  finally
    Strs.Free;
  end;
end;

function TInterfaceInfo.GetSize: integer;
begin
  Result := SizeOf(FAddress) +
    SizeOf(FFlag) +
    SizeOf(FMask);
end;

function TInterfaceInfo.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := ABuffer.ContainData(Size);
  if not Result then
    Exit;

  Address.SetAddress(ABuffer.ReadCardinal);
  Flag := ABuffer.ReadCardinal;
  Mask.SetAddress(ABuffer.ReadCardinal);
end;

function TInterfaceInfo.LoadFromFile(AFile: TIOBuffer): boolean;
var
  TagId: integer;
begin
  TagId := AFile.ReadTag;

  while True do
  begin
    case TagId of
      CPInterfaceInfo_Address_Id:
        begin
          AFile.ReadType;
          Address.SetAddress(AFile.ReadCardinal);
        end;

      CPInterfaceInfo_Flag_Id:
        begin
          AFile.ReadType;
          Flag := AFile.ReadCardinal;
        end;

      CPInterfaceInfo_Mask_Id:
        begin
          AFile.ReadType;
          Mask.SetAddress(AFile.ReadCardinal);
        end
    else
      Break;
    end;

    TagId := AFile.ReadTag;
  end;

  Result := True;
end;

procedure TInterfaceInfo.SaveProperty(AFile: TIOBuffer);
begin
  inherited;

  AFile.WriteProp(CPInterfaceInfo_Address_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(Address.Base);

  AFile.WriteProp(CPInterfaceInfo_Flag_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(Flag);

  AFile.WriteProp(CPInterfaceInfo_Mask_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(Mask.Base);
end;

procedure TInterfaceInfo.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteCardinal(Address.Base);
  ABuffer.WriteCardinal(Flag);
  ABuffer.WriteCardinal(Mask.Base);
end;

procedure TInterfaceInfo.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTInterfaceInfo_Id);
end;

{ TInterfacesInfo }

constructor TInterfacesInfo.Create;
begin
  inherited;
  FInterfaces := TNetObjectList<TInterfaceInfo>.Create;
end;

destructor TInterfacesInfo.Destroy;
begin
  FInterfaces.Free;
  inherited;
end;

function TInterfacesInfo.Equal(ANetItem: TNetItem): boolean;
var
  Src: TInterfacesInfo;
begin
  Result := False;
  if not (ANetItem is TInterfacesInfo) then
    Exit;
  Src := TInterfacesInfo(ANetItem);

  Result := Interfaces.Equal(Src.Interfaces);
end;

function TInterfacesInfo.GetSize: integer;
begin
  Result := inherited GetSize + Interfaces.Size;
end;

function TInterfacesInfo.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then
    Exit;

  Interfaces.LoadFromBuffer(ABuffer);
end;

function TInterfacesInfo.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := True;

  case ATagId of
    CPInterfacesInfo_Interfaces_Id:
      begin
        Interfaces.Items.Clear;
        Interfaces.LoadFromFile(AFile);
      end;

  else
    Result := inherited LoadProperty(ATagId, ATypeId, AFile);
  end;
end;

procedure TInterfacesInfo.SaveProperty(AFile: TIOBuffer);
begin
  inherited;

  if Interfaces.HasItems then
  begin
    AFile.WriteProp(CPInterfacesInfo_Interfaces_Id);
    Interfaces.SaveToFile(AFile);
  end;
end;

procedure TInterfacesInfo.SaveToBuffer(ABuffer: TIOBuffer);
begin
  Interfaces.SaveToBuffer(ABuffer);
end;

procedure TInterfacesInfo.RequestInterfaces;
var
  I: integer;
  InterList: Net.Utils.TInterfaceArray;
  Inter: TInterfaceInfo;
begin
  Interfaces.Items.Clear;

  try
    SetLength(InterList, 10);
    Net.Utils.EnumInterfaces(InterList);

    for I := 0 to Length(InterList)-1 do
    begin
      Inter := Interfaces.Add;

      with Inter, InterList[I] do
      begin
        Flag := iiFlags;
        Address.SetAddress(iiAddress.AddressIn.sin_addr.S_addr);
        Mask.SetAddress(iiNetmask.AddressIn.sin_addr.S_addr);
      end;
    end;
  finally
    SetLength(InterList, 0);
  end;
end;

procedure TInterfacesInfo.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTInterfacesInfo_Id);
end;

{ TConnectInfo }

constructor TConnectInfo.Create;
begin
  inherited;
  FInterfaceInfo := TInterfacesInfo.Create;
end;

destructor TConnectInfo.Destroy;
begin
  FInterfaceInfo.Free;
  inherited;
end;

function TConnectInfo.Equal(ANetItem: TNetItem): boolean;
var
  Src: TConnectInfo;
begin
  Result := False;
  if not (ANetItem is TConnectInfo) then
    Exit;
  Src := TConnectInfo(ANetItem);

  Result := (Access = Src.Access) and
    (Account = Src.Account) and
    (Address.Base = Src.Address.Base) and
    (Application = Src.Application) and
    (ComputerName = Src.ComputerName) and
    (Extention = Src.Extention) and
    InterfaceInfo.Equal(Src.InterfaceInfo) and
    (NetLibraryInternalVersion = Src.NetLibraryInternalVersion) and
    (NetLibraryVersion = Src.NetLibraryVersion) and
    (NetLibraryDate = Src.NetLibraryDate) and
    (Port = Src.Port) and
    (Timeout = Src.Timeout) and
    (User = Src.User);
end;

function TConnectInfo.GetSize: integer;
begin
  Result :=
    SizeOf(FAccess) +
    GetStringSerializeLength(FAccount) +
    SizeOf(FAddress) +
    GetStringSerializeLength(FApplication) +

    GetStringSerializeLength(FComputerName) +
    SizeOf(FExtention) +
    InterfaceInfo.Size +

    SizeOf(FNetLibraryDate) +
    SizeOf(FNetLibraryVersion) +
    SizeOf(FPort) +
    SizeOf(FTimeout) +
    GetStringSerializeLength(FUser);
end;

function TConnectInfo.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  try
    FAccess := ABuffer.ReadInteger;
    FAccount := ABuffer.ReadString;
    FAddress.Base := ABuffer.ReadCardinal;
    FApplication := ABuffer.ReadString;

    FComputerName := ABuffer.ReadString;
    FExtention := ABuffer.ReadInteger;
    InterfaceInfo.LoadFromBuffer(ABuffer);

    FNetLibraryDate := ABuffer.ReadDateTime;
    FNetLibraryVersion := ABuffer.ReadCardinal;
    FNetLibraryInternalVersion := ABuffer.ReadCardinal;

    FPort := ABuffer.ReadSmallInt;
    FTimeout := ABuffer.ReadInteger;
    FUser := ABuffer.ReadString;

    Result := Valid;
  except
    Result := False;
  end;
end;

function TConnectInfo.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := True;

  case ATagId of
    CPConnectInfo_Access_Id:
      Access := AFile.ReadInteger;

    CPConnectInfo_Account_Id:
      Account := AFile.ReadString;

    CPConnectInfo_Address_Id:
      Address.SetAddress(AFile.ReadCardinal);

    CPConnectInfo_Application_Id:
      Application := AFile.ReadString;

    CPConnectInfo_ComputerName_Id:
      ComputerName := AFile.ReadString;

    CPConnectInfo_Extention_Id:
      Extention := AFile.ReadInteger;

    CPConnectInfo_InterfaceInfo:
      InterfaceInfo.LoadFromFile(AFile);

    CPConnectInfo_NetLibraryDate_Id:
      NetLibraryDate := AFile.ReadDateTime;

    CPConnectInfo_NetLibraryVersion_Id:
      NetLibraryVersion := AFile.ReadCardinal;

    CPConnectInfo_NetLibraryInternalVersion_Id:
      NetLibraryInternalVersion := AFile.ReadCardinal;

    CPConnectInfo_Port_Id:
      FPort := AFile.ReadWord;

    CPConnectInfo_Timeout_Id:
      Timeout := AFile.ReadInteger;

    CPConnectInfo_User_Id:
      User := AFile.ReadString;
  else
    Result := inherited LoadProperty(ATagId, ATypeId, AFile);
  end;
end;

procedure TConnectInfo.SaveProperty(AFile: TIOBuffer);
begin
  inherited;

  AFile.WriteProp(CPConnectInfo_Access_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(Access);

  AFile.WriteProp(CPConnectInfo_Account_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(Account);

  AFile.WriteProp(CPConnectInfo_Address_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(Address.Base);

  AFile.WriteProp(CPConnectInfo_Application_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(Application);

  AFile.WriteProp(CPConnectInfo_ComputerName_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(ComputerName);

  AFile.WriteProp(CPConnectInfo_Extention_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(Extention);

  AFile.WriteProp(CPConnectInfo_InterfaceInfo);
  InterfaceInfo.SaveToFile(AFile);

  AFile.WriteProp(CPConnectInfo_NetLibraryDate_Id);
  AFile.WriteType(CDateTimeType_Id);
  AFile.WriteDateTime(NetLibraryDate);

  AFile.WriteProp(CPConnectInfo_NetLibraryInternalVersion_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(NetLibraryInternalVersion);

  AFile.WriteProp(CPConnectInfo_NetLibraryVersion_Id);
  AFile.WriteType(CCardinalType_Id);
  AFile.WriteCardinal(NetLibraryVersion);

  AFile.WriteProp(CPConnectInfo_Port_Id);
  AFile.WriteType(CWordType_Id);
  AFile.WriteWord(Port);

  AFile.WriteProp(CPConnectInfo_Timeout_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(Timeout);

  AFile.WriteProp(CPConnectInfo_User_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(User);
end;

procedure TConnectInfo.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteInteger(FAccess);
  ABuffer.WriteString(FAccount);
  ABuffer.WriteCardinal(FAddress.Base);
  ABuffer.WriteString(FApplication);

  ABuffer.WriteString(FComputerName);
  ABuffer.WriteInteger(FExtention);
  InterfaceInfo.SaveToBuffer(ABuffer);

  ABuffer.WriteDateTime(FNetLibraryDate);
  ABuffer.WriteCardinal(FNetLibraryVersion);
  ABuffer.WriteCardinal(FNetLibraryInternalVersion);
  ABuffer.WriteSmallInt(FPort);
  ABuffer.WriteInteger(FTimeout);
  ABuffer.WriteString(FUser);
end;

procedure TConnectInfo.SetAddress(const AAddr: TSockAddrIn);
begin
  FAddress.Base := AAddr.sin_addr.S_addr;
  FPort := SmallInt(AAddr.sin_port shl 8 or AAddr.sin_port shr 8);
end;

function TConnectInfo.Valid: boolean;
begin
  Result := IsIntValid(Access, CI_ACCESS_PROHIBIT, CI_ACCESS_WRITE) and
    IsStringValid(Account, CI_ACCESS_APPLICATION_NAME_MAX_SIZE) and
    IsStringValid(Application, MAX_PATH) and
    IsStringValid(ComputerName, CI_ACCESS_COMPUTER_NAME_MAX_SIZE) and
    IsStringValid(User, CI_ACCESS_USER_NAME_MAX_SIZE);
end;

procedure TConnectInfo.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTConnectInfo_Id);
end;

{ TConnectAcceptInfo }

function TConnectAcceptInfo.Equal(ANetItem: TNetItem): boolean;
var
  Src: TConnectAcceptInfo;
begin
  Result := False;
  if not (ANetItem is TConnectAcceptInfo) then
    Exit;
  Src := TConnectAcceptInfo(ANetItem);

  Result := SessionID = Src.SessionID;
end;

function TConnectAcceptInfo.GetSize: integer;
begin
  Result := SizeOf(SessionID);
end;

function TConnectAcceptInfo.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  FSessionID := ABuffer.ReadInteger;
end;

procedure TConnectAcceptInfo.SaveToBuffer(ABuffer: TIOBuffer);
begin
  inherited;
  ABuffer.WriteInteger(SessionID);
end;

{ TAcceptHash }

function TAcceptHash.Equal(ANetItem: TNetItem): boolean;
var
  Src: TAcceptHash;
begin
  Result := False;
  if not (ANetItem is TAcceptHash) then
    Exit;
  Src := TAcceptHash(ANetItem);

  Result := (PasswordBase = Src.PasswordBase) and
    (ReplyBase = Src.ReplyBase) and
    (ReplyInterval = Src.ReplyInterval) and
    (RequestBase = Src.RequestBase) and
    (RequestInterval = Src.RequestInterval);
end;

procedure TAcceptHash.Generate;
begin
  Randomize;
  FPasswordBase := Random($7fffffff);
  FReplyBase := Random(128) shl 1 + 128;
  FReplyInterval := Random(4) shl 2 + 1;
  FRequestBase := Random(128);
  FRequestInterval := Random(4) shl 1 + 3;
end;

function TAcceptHash.GetSize: integer;
begin
  Result :=
    SizeOf(FPasswordBase) +
    SizeOf(FReplyBase) +
    SizeOf(FReplyInterval) +
    SizeOf(FRequestBase) +
    SizeOf(FRequestInterval);
end;

function TAcceptHash.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  try
    FPasswordBase := ABuffer.ReadCardinal;
    FReplyBase := ABuffer.ReadCardinal;
    FReplyInterval := ABuffer.ReadCardinal;
    FRequestBase := ABuffer.ReadCardinal;
    FRequestInterval := ABuffer.ReadCardinal;
  except
    Result := False;
  end;
end;

procedure TAcceptHash.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.WriteCardinal(PasswordBase);
  ABuffer.WriteCardinal(ReplyBase);
  ABuffer.WriteCardinal(ReplyInterval);
  ABuffer.WriteCardinal(RequestBase);
  ABuffer.WriteCardinal(RequestInterval);
end;

{ TAcceptKey }

function TAcceptKey.Equal(ANetItem: TNetItem): boolean;
var
  Src: TAcceptKey;
begin
  Result := False;
  if not (ANetItem is TAcceptKey) then
    Exit;
  Src := TAcceptKey(ANetItem);

  Result := System.SysUtils.CompareMem(@ReplyKey, @Src.ReplyKey, SizeOf(ReplyKey)) and
    System.SysUtils.CompareMem(@RequestKey, @Src.RequestKey, SizeOf(RequestKey));
end;

procedure TAcceptKey.Generate;
begin
  Randomize;
  PInteger(@FReplyKey[0])^ := Random(MaxInt);
  PInteger(@FReplyKey[4])^ := Random(MaxInt);
  PInteger(@FReplyKey[8])^ := Random(MaxInt);
  PInteger(@FReplyKey[12])^ := Random(MaxInt);

  PInteger(@FRequestKey[0])^ := Random(MaxInt);
  PInteger(@FRequestKey[4])^ := Random(MaxInt);
  PInteger(@FRequestKey[8])^ := Random(MaxInt);
  PInteger(@FRequestKey[12])^ := Random(MaxInt);
end;

function TAcceptKey.GetSize: integer;
begin
  Result := SizeOf(FReplyKey) + SizeOf(FRequestKey);
end;

function TAcceptKey.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  try
    ABuffer.Read(FReplyKey, SizeOf(FReplyKey));
    ABuffer.Read(FRequestKey, SizeOf(FRequestKey));
  except
    Result := False;
  end;
end;

procedure TAcceptKey.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.Write(FReplyKey, SizeOf(FReplyKey));
  ABuffer.Write(FRequestKey, SizeOf(FRequestKey));
end;

{ TAuthorizeInfo }

function TAuthorizeInfo.Equal(ANetItem: TNetItem): boolean;
var
  Src: TAuthorizeInfo;
begin
  Result := False;
  if not (ANetItem is TAuthorizeInfo) then
    Exit;
  Src := TAuthorizeInfo(ANetItem);

  Result := System.SysUtils.CompareMem(@HashCode, @Src.HashCode, SizeOf(HashCode));
end;

procedure TAuthorizeInfo.Generate(const APassword: string);
var
  Stream: TStream;
begin
  if APassword = '' then
    FillChar(FHashCode, SizeOf(FHashCode), 0)
  else
  begin
    Stream := TStringStream.Create(APassword);

    try
      GetSHA512Hash(Stream, FPassHash);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TAuthorizeInfo.GetHash(const APassHash: TSHA512HashCode; var AHashCode: THashCode);
var
  Hash: TNetHash;
begin
  Hash := TNetHash.Create;

  try
    Hash.Base := Base;
    Hash.GetHash(@APassHash[0], SizeOf(APassHash), AHashCode);
  finally
    Hash.Free;
  end;
end;

procedure TAuthorizeInfo.SetHashCode;
begin
  GetHash(FPassHash, FHashCode);
end;

function TAuthorizeInfo.GetSize: integer;
begin
  Result := SizeOf(FHashCode);
end;

function TAuthorizeInfo.HashValid(const APassHash: TSHA512HashCode): boolean;
var
  H: THashCode;
begin
  GetHash(APassHash, H);
  Result :=
    (HashCode[0] = H[0]) and
    (HashCode[1] = H[1]) and
    (HashCode[2] = H[2]) and
    (HashCode[3] = H[3]);
end;

function TAuthorizeInfo.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  try
    ABuffer.Read(FHashCode, SizeOf(FHashCode));
  except
    Result := False;
  end;
end;

function TAuthorizeInfo.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := True;

  case ATagId of
//    CPAuthorizeInfo_Base_Id:
//      Base := AFile.ReadCardinal;

    CPAuthorizeInfo_Hash_Id:
      AFile.Read(FHashCode, SizeOf(HashCode));

  else
    Result := inherited LoadProperty(ATagId, ATypeId, AFile);
  end;
end;

procedure TAuthorizeInfo.SaveProperty(AFile: TIOBuffer);
begin
  inherited;

//  AFile.WriteProp(CPAuthorizeInfo_Base_Id);
//  AFile.WriteType(CCardinalType_Id);
//  AFile.WriteCardinal(Base);

  AFile.WriteProp(CPAuthorizeInfo_Hash_Id);
  AFile.WriteType(CBufferType_Id);
  AFile.Write(HashCode, SizeOf(HashCode));
end;

procedure TAuthorizeInfo.SaveToBuffer(ABuffer: TIOBuffer);
begin
  ABuffer.Write(FHashCode, SizeOf(FHashCode));
end;

procedure TAuthorizeInfo.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTAuthorizeInfo_Id);
end;

{ TNetBuffer<PacketT, LocalT> }

constructor TNetBuffer<PacketT, LocalT>.Create(ANetChanger: TNetChanger<LocalT>);
begin
  FNetChanger := ANetChanger;
  FBuffer := TIOMemory.Create;
  FBuffer.Size := DEFAULT_BUFFER_SIZE;
  FNetPacket := PacketT.Create;
  FHashed := False;
  FEncrypted := False;
  SetHashed(True);
end;

destructor TNetBuffer<PacketT, LocalT>.Destroy;
begin
  FNetHash.Free;
  FNetPacket.Free;
  FBuffer.Free;
  FCryptoProvider.Free;
  inherited;
end;

function TNetBuffer<PacketT, LocalT>.NetBufferSize: integer;
begin
  Result := 0;
end;

function TNetBuffer<PacketT, LocalT>.Receive: boolean;
var
  PacketSize: integer;
  DataOffset: integer;
  DataSize: integer;
begin
  if NetChanger.TcpClient.PeekBuf(PacketSize, SizeOf(PacketSize)) = SOCKET_ERROR then
  begin
    NetChanger.NotifyError(NCH_CONNECTION_CLOSED, nil);
    Exit(False);
  end;

  if PacketSize <= 0 then
  begin
    NetChanger.TcpClient.Close;
    if PacketSize = -WSAETOOMANYREFS then
      NetChanger.NotifyError(NCH_MAX_CONNECTION_EXCEEDED, nil)
    else
      NetChanger.NotifyError(NCH_ERROR, nil);

    Exit(False);
  end;

  Buffer.Size := PacketSize;
  Result := NetChanger.Receive(Buffer.Memory^, PacketSize);
  if not Result then
  begin
    NetChanger.NotifyError(NCH_RECEIVE_ERROR, nil);
    Exit(False);
  end;

  if Hashed then
  begin
    DataOffset := SizeOf(Integer) + SizeOf(THashCode);
    DataSize := Buffer.Size - SizeOf(Integer) - SizeOf(THashCode);

    if (DataOffset >= Buffer.Size) or (DataSize < 0) or
      not NetHash.Valid(Buffer.GetPtr(DataOffset), DataSize, PHashCode(Buffer.GetPtr(SizeOf(Integer)))^) then
    begin
      NetChanger.NotifyError(NCH_INVALID_TOKEN, nil);
      NetChanger.TcpClient.Close;
    end
    else
      FNetHash.NextBase;

    Buffer.Seek(SizeOf(Integer) + SizeOf(THashCode));
    if Encrypted then
    begin
      Buffer.Decrypt(CryptoProvider);
      Buffer.Seek(SizeOf(Integer) + SizeOf(THashCode));
    end;
  end
  else
    Buffer.Seek(SizeOf(Integer));

  NetPacket.LoadFromBuffer(Buffer);
end;

function TNetBuffer<PacketT, LocalT>.Send: boolean;
begin
  if Hashed then
  begin
    // set buffer size
    Buffer.Size := SizeOf(Integer) + SizeOf(THashCode) + NetPacket.Size;

    // copy net packet to buffer
    Buffer.Seek(SizeOf(Integer) + SizeOf(THashCode));
    NetPacket.SaveToBuffer(Buffer);

    if Encrypted then
    begin
      Buffer.Seek(SizeOf(Integer) + SizeOf(THashCode));
      Buffer.Encrypt(CryptoProvider);
    end;

    // set hash
    NetHash.GetHash(Buffer.GetPtr(SizeOf(Integer) + SizeOf(THashCode)),
      Buffer.Size - SizeOf(Integer) - SizeOf(THashCode),
      PHashCode(Buffer.GetPtr(SizeOf(Integer)))^);
    NetHash.NextBase;

    // set buffer size
    Buffer.Seek();
    Buffer.WriteInteger(Buffer.Size);
  end
  else
  begin
    Buffer.Size := SizeOf(Integer) + NetPacket.Size;
    Buffer.Seek(SizeOf(Integer));
    NetPacket.SaveToBuffer(Buffer);

    Buffer.Seek();
    Buffer.WriteInteger(Buffer.Size);
  end;

  Result := NetChanger.Send(Buffer.Memory^, Buffer.Size);
  if not Result then
    NetChanger.NotifyError(NCH_SEND_ERROR, nil);
end;

procedure TNetBuffer<PacketT, LocalT>.SetCryptoProvider(ACryptoProvider: TCryptoProvider);
begin
  if FCryptoProvider <> nil then
    FCryptoProvider.Free;

  FCryptoProvider := ACryptoProvider;
  FEncrypted := FCryptoProvider <> nil;

  if Encrypted then
    Hashed := True;
end;

procedure TNetBuffer<PacketT, LocalT>.SetHashed(const Value: boolean);
begin
  if Value <> FHashed then
  begin
    if not Value and Encrypted then
      Exit;

    FHashed := Value;
    if Value then
      FNetHash := TNetHash.Create
    else
    begin
      FNetHash.Free;
      FNetHash := nil;
    end;
  end;
end;

function TNetBuffer<PacketT, LocalT>.CheckConnection: boolean;
var
  Res: integer;
begin
  NetChanger.TcpClient.PeekBuf(Res, SizeOf(Res));
  if Res = -WSAETOOMANYREFS then
    NetChanger.NotifyError(NCH_MAX_CONNECTION_EXCEEDED, nil)
  else
    NetChanger.NotifyError(NCH_ERROR, nil);
end;

{ TRequestBuffer<LocalT> }

function TRequestBuffer<LocalT>.NetBufferSize: integer;
begin
  Result := NetChanger.FMaxReceiveBufferSize;
end;

{ TReplyBuffer<LocalT> }

function TReplyBuffer<LocalT>.NetBufferSize: integer;
begin
  Result := NetChanger.MaxSendBufferSize;
end;

{ TNetChanger<LocalT> }

constructor TNetChanger<LocalT>.Create(ATcpServer: TTcpServer<LocalT>; ATcpClient: TTcpClient<LocalT>);
begin
  FMaxReceiveBufferSize := 1;
  FMaxSendBufferSize := 1;
  FTcpClient := ATcpClient;
  FTcpServer := ATcpServer;
  FRequestBuffer := TRequestBuffer<LocalT>.Create(self);
  FReplyBuffer := TReplyBuffer<LocalT>.Create(self);
  HandleBuffer;
end;

destructor TNetChanger<LocalT>.Destroy;
begin
  FRequestBuffer.Free;
  FReplyBuffer.Free;
  inherited;
end;

function TNetChanger<LocalT>.Receive(var Buffer; Size: integer): boolean;
var
  Res: integer;
  BytesRec: integer;
  BufferSize: integer;
  Ptr: Pointer;
begin
  BytesRec := 0;
  BufferSize := FMaxReceiveBufferSize;
  Ptr := @Buffer;

  repeat
    if (Size - BytesRec) < BufferSize then
      BufferSize := Size - BytesRec;

    Res := TcpClient.ReceiveBuf(Ptr^, BufferSize);

    BytesRec := BytesRec + Res;
    NativeInt(Ptr) := NativeInt(Ptr) + Res;

  until (Res <= 0) or (BytesRec = Size);

  Result := Res > 0;
end;

function TNetChanger<LocalT>.Send(var Buffer; Size: integer): boolean;
var
  Res: integer;
  BytesSnd: integer;
  BufferSize: integer;
  Ptr: Pointer;
begin
  BytesSnd := 0;
  BufferSize := FMaxSendBufferSize;
  Ptr := @Buffer;

  repeat
    if (Size - BytesSnd) < BufferSize then
      BufferSize := Size - BytesSnd;

    Res := TcpClient.SendBuf(Ptr^, BufferSize);

    BytesSnd := BytesSnd + Res;
    NativeInt(Ptr) := NativeInt(Ptr) + Res;

  until (Res <= 0) or (BytesSnd = Size);

  Result := Res > 0;
end;

procedure TNetChanger<LocalT>.HandleBuffer;
var
  MessageSize: Cardinal;
begin
  if TcpClient.Active then
  begin
    if (TcpClient.GetOption(SO_RCVBUF, @FMaxReceiveBufferSize, SizeOf(FMaxReceiveBufferSize)) <> 0) or
      (FMaxReceiveBufferSize <= 0) then
    begin
      TcpClient.Close;
      Exit;
    end;

    if (TcpClient.GetOption(SO_SNDBUF, @FMaxSendBufferSize, SizeOf(FMaxSendBufferSize)) <> 0) or
      (FMaxSendBufferSize <= 0) then
    begin
      TcpClient.Close;
      Exit;
    end;

    if (TcpClient.GetOption(SO_MAX_MSG_SIZE, @MessageSize, SizeOf(MessageSize)) <> 0) or
      (MessageSize = 0) then
    begin
      TcpClient.Close;
      Exit;
    end;

    if Cardinal(FMaxSendBufferSize) > MessageSize then
      FMaxSendBufferSize := MessageSize;
  end;
end;

procedure TNetChanger<LocalT>.NotifyError(ANetChangerError: Integer; AErrObj: TObject);
begin
  if Assigned(FOnNetChangerError) then
    FOnNetChangerError(self, ANetChangerError, AErrObj);
end;

{ TServerCmd<GlobalT, LocalT> }

constructor TServerCmd<GlobalT, LocalT>.Create(ACommand: TPacketCommand; AServerParser: TCommandParser<GlobalT, LocalT>);
begin
  FServerParser := AServerParser;
  FCommand := ACommand;
  FServerParser.Add(self);
end;

procedure TServerCmd<GlobalT, LocalT>.Execute(ACommandServer: TCommandServer<GlobalT, LocalT>);
begin
  ACommandServer.ReplyBuffer.NetPacket.Status := NC_NOP;
end;

procedure TServerCmd<GlobalT, LocalT>.PostExecute(ACommandServer: TCommandServer<GlobalT, LocalT>);
begin
end;

{ TCommandParser<GlobalT, LocalT> }

function TCommandParser<GlobalT, LocalT>.Add(ACmd: TServerCmd<GlobalT, LocalT>): integer;
begin
  {$IFDEF DEBUG}
  if IndexOf(ACmd.Command) >= 0 then
    raise ECommandParser.CreateFmt('Команда (0x%x) уже существует!', [Integer(ACmd.Command)]);
  {$ENDIF}

  Result := FItems.Add(ACmd);
end;

procedure TCommandParser<GlobalT, LocalT>.Clear;
begin
  FItems.Clear;
end;

constructor TCommandParser<GlobalT, LocalT>.Create(ACommandServer: TCommandServer<GlobalT, LocalT>);
begin
  FCommandServer := ACommandServer;
  FItems := TObjectList<TServerCmd<GlobalT, LocalT>>.Create;
end;

destructor TCommandParser<GlobalT, LocalT>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCommandParser<GlobalT, LocalT>.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TCommandParser<GlobalT, LocalT>.GetServerHandler(const ACmd: TPacketCommand): TServerCmd<GlobalT, LocalT>;
var
  Index: integer;
begin
  Index := IndexOf(ACmd);

  if Index >= 0 then
  begin
    FItems.Move(Index, 0);
    Exit(FItems.First);
  end
  else
    Result := nil;
end;

function TCommandParser<GlobalT, LocalT>.IndexOf(const ACmd: TPacketCommand): integer;
var
  Index: integer;
  ServerCmd: TServerCmd<GlobalT, LocalT>;
begin
  for Index := 0 to FItems.Count - 1 do
  begin
    ServerCmd := FItems[Index];

    if ServerCmd.Command = ACmd then
      Exit(Index);
  end;

  Result := -1;
end;

procedure TCommandParser<GlobalT, LocalT>.Remove(ACmd: TPacketCommand);
var
  Index: integer;
begin
  Index := IndexOf(ACmd);

  if Index >= 0 then
    FItems.Delete(Index);
end;

{ TCommandServer<GlobalT, LocalT> }

constructor TCommandServer<GlobalT, LocalT>.Create(AGlobalData: GlobalT; ATcpServer: TTcpServer<LocalT>; ATcpClient: TTcpClient<LocalT>);
begin
  inherited Create(ATcpServer, ATcpClient);
  FParser := TCommandParser<GlobalT, LocalT>.Create(self);
  FGlobalData := AGlobalData;
end;

destructor TCommandServer<GlobalT, LocalT>.Destroy;
begin
  FParser.Free;
  inherited;
end;

{ TNetServer<GlobalT, LocalT> }

function TNetServer<GlobalT, LocalT>.Realize: boolean;
var
  ServerCmd: TServerCmd<GlobalT, LocalT>;
begin
  if not RequestBuffer.Receive then
    Exit(False);

  ReplyBuffer.NetPacket.Status := NC_UNKNOWN_COMMAND;

  ServerCmd := Parser.GetServerHandler(RequestBuffer.NetPacket.Command);
  if ServerCmd <> nil then
  try
    ServerCmd.UsePostExecution := False;
    ServerCmd.Execute(self);
  except
    on E: Exception do
    begin
      ReplyBuffer.NetPacket.Status := NC_FATAL_ERROR;
      NotifyError(NCH_EXCEPTION, E);
    end;
  end;

  if not ReplyBuffer.Send then
    Exit(False);

  if (ServerCmd <> nil) and ServerCmd.UsePostExecution then
    ServerCmd.PostExecute(self);

  RequestBuffer.NetPacket.Release;
  ReplyBuffer.NetPacket.Release;

  Result := True;
end;

{ TNetClient<GlobalT, LocalT> }

function TNetClient<GlobalT, LocalT>.Realize: boolean;
begin
  if not RequestBuffer.Send then
    Exit(False);

  if not ReplyBuffer.Receive then
    Exit(False);

  Result := True;
end;

end.
