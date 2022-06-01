unit Net.Client;

interface

uses
  System.Types, System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.Windows,
  Net.Socket, Net.Stream, Net.Svr, Net.SvrData, Net.Data, Net.Cnst, Net.ClientCnst;
//  , IsExt;

const
  // used port numbers
  DEFAULT_CONFIG_PORT             = ;
  DEFAULT_DATA_PORT               = ;
  DEFAULT_TIMEOUT                 = 30;
  DEFAULT_ACCESS                  = CI_ACCESS_READ;
  DEFAULT_MODE                    = IC_MODE_DATA;
  DEFAULT_USE_COMPRESSION         = True;
  DEFAULT_DELTA_COUNT             = 64;

  // Infosys Client Events
  ICE_CONNECTION_CLOSED           = 0;

  ACCESS_COUNT = 2;
  SAccess: array[0..ACCESS_COUNT-1] of string = (IC_ACCESS_READ, IC_ACCESS_WRITE);
  AAccess: array[0..ACCESS_COUNT-1] of integer = (CI_ACCESS_READ, CI_ACCESS_WRITE);

type
  TClientData = class(TNetItem)
  private
    FAuthorizeInfo: TAuthorizeInfo;
    FConfigPort: string;
    FConnectInfo: TConnectInfo;
    FDataBuffer: TParamDataBuffer;
    FDataPort: string;
    FMode: string;
    FNetChangerError: integer;
    FParamCount: integer;
    FSessionID: integer;
    FServer: string;
    FSocketError: integer;
    FWriteInfo: TWriteReplyInfo;
    function GetAccess(const Str: string): integer;
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
  protected
    function LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean; override;
    procedure SaveProperty(AFile: TIOBuffer); override;
    procedure WriteClass(AFile: TIOBuffer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(ANetItem: TNetItem): boolean; override;
    procedure SetPassword(const APassword: string);
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property AuthorizeInfo: TAuthorizeInfo read FAuthorizeInfo;
    property ConfigPort: string read FConfigPort;
    property ConnectInfo: TConnectInfo read FConnectInfo;
    property DataBuffer: TParamDataBuffer read FDataBuffer;
    property DataPort: string read FDataPort;
    property Mode: string read FMode;
    property NetChangerError: integer read FNetChangerError write FNetChangerError;
    property ParamCount: integer read FParamCount write FParamCount;
    property SessionID: integer read FSessionID write FSessionID;
    property Server: string read FServer;
    property SocketError: integer read FSocketError write FSocketError;
    property WriteInfo: TWriteReplyInfo read FWriteInfo;
  end;

  TProvider = class;
  TInfoNetClient = class(TNetClient<TProvider, TClientData>)
  public
    constructor Create(AData: TProvider; ATcpServer: TTcpServer<TClientData>; ATcpClient: TTcpClient<TClientData>); override;
    function Query(ACmd: TPacketCommand; ARequest, AReply: TNetItem): integer;
  end;

  THashSetKeyCmd = class(TServerCmd<TProvider, TClientData>)
  protected
    procedure Execute(ACommandServer: TCommandServer<TProvider, TClientData>); override;
  end;

  THashClearCmd = class(TServerCmd<TProvider, TClientData>)
  protected
    procedure Execute(ACommandServer: TCommandServer<TProvider, TClientData>); override;
  end;

  TSetCypherKeyCmd = class(TServerCmd<TProvider, TClientData>)
  protected
    procedure Execute(ACommandServer: TCommandServer<TProvider, TClientData>); override;
  end;

  TCallBackProc = procedure (Handle: THandle; Event: integer; Data: Pointer) of object;

//  TIsClientOption = (ioExecuteExternalCommands, ioGrantSplitDestinations);
//  TIsClientOptions = set of TIsClientOption;

  TInfoClient = class
  private
    FNetClient: TInfoNetClient;
    FProvider: TProvider;
    FTcpClient: TTcpClient<TClientData>;
    procedure OnNetChangerError(Sender: TObject; AError: Integer; AErrObj: TObject);
    procedure OnSocketError(Sender: TObject; ASocketError: Integer);
  public
    constructor Create(AProvider: TProvider);
    destructor Destroy; override;
    function Open(AConnectionString, APassword: PChar): integer;
    procedure Close;
    property NetClient: TInfoNetClient read FNetClient;
    property Provider: TProvider read FProvider;
    property TcpClient: TTcpClient<TClientData> read FTcpClient;
  end;

  TExtendedCmdThread = class(TThread)
  private
    FInfoClient: TInfoClient;
//    FReserveServerName: string;
  public
    constructor Create(AInfoClient: TInfoClient);
    procedure Execute; override;
  end;

  TClientExtendedCommand = class
  private
    FInfoClient: TInfoClient;
//    FExtendedCmd: TNetClient;
//    procedure CreateReserveServer;
  public
    constructor Create(AInfoClient: TInfoClient);
    destructor Destroy; override;
    procedure Execute;
  end;

  TSubscriberReadRec9 = record

  end;

  TProvider = class
  private
//    FCallBackProc: TCallBackProc;
    FFreeOnCloseConnection: boolean;
    FItems: TList<TInfoClient>;
    FCurrentExtension: integer;
    function GetCount: integer;
    function GetClient(Index: integer): TInfoClient;
  protected
    procedure AddClient(AClient: TInfoClient);
    procedure RemoveClient(AClient: TInfoClient);
    procedure TerminateClients;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateClient: TInfoClient;
    function IndexOf(AClient: TInfoClient): integer;


    function ReadParam(const ReadRec: TSubscriberReadRec9; Buffer: Pointer; var BufferSize: Cardinal): integer;
    function ReadParams(Buffer: Pointer; BufferSize: integer): integer;
    function Reset(const DateTime: TDateTime): integer;
    function Write(Buffer: Pointer; BufferSize: integer): integer;


    procedure ClearClients;
    procedure SetConnectionString(const AConnectionString: string);
    property Client[Index: integer]: TInfoClient read GetClient;
    property Count: integer read GetCount;
    property FreeOnCloseConnection: boolean read FFreeOnCloseConnection write FFreeOnCloseConnection;
//    property CallBackProc: TCallBackProc read FCallBackProc write FCallBackProc;
  end;

implementation

uses
  Net.Utils;
//  WinSock, SockCnst, ClientSck, CryptUnit, IsUtil
  //IsConLib;

{ TClientData }

constructor TClientData.Create;
begin
  inherited;
  FAuthorizeInfo := TAuthorizeInfo.Create;
  FConfigPort := DEFAULT_CONFIG_PORT;

  FConnectInfo := TConnectInfo.Create;
  ConnectInfo.Account := Net.Utils.UserAccount;
  ConnectInfo.Application := Net.Utils.InstanceName;
  ConnectInfo.ComputerName := Net.Utils.ComputerName;

  FConnectInfo.Access := CI_ACCESS_READ;
  FConnectInfo.Timeout := DEFAULT_TIMEOUT;
  FDataBuffer := TParamDataBuffer.Create;
  FDataPort := DEFAULT_DATA_PORT;
  FMode := DEFAULT_MODE;
  FWriteInfo := TWriteReplyInfo.Create;
end;

//  MODE_COUNT = 2;
//  SMode: array[0..MODE_COUNT-1] of string = (IC_MODE_DATA, IC_MODE_DATA);
//  AMode: array[0..MODE_COUNT-1] of integer = (SBA_READ, SBA_WRITE);

destructor TClientData.Destroy;
begin
  FAuthorizeInfo.Free;
  FConnectInfo.Free;
  FDataBuffer.Free;
  FWriteInfo.Free;
  inherited;
end;

function TClientData.Equal(ANetItem: TNetItem): boolean;
var
  Src: TClientData;
begin
  Result := False;
  if not (ANetItem is TClientData) then
    Exit;
  Src := TClientData(ANetItem);

  Result := AuthorizeInfo.Equal(Src.AuthorizeInfo) and
    (ConfigPort = Src.ConfigPort) and
    ConnectInfo.Equal(Src.ConnectInfo) and
    (DataPort = Src.DataPort) and
    (Mode = Src.Mode) and
    (NetChangerError = Src.NetChangerError) and
    (ParamCount = Src.ParamCount) and
    (SessionID = Src.SessionID) and
    (Server = Src.Server) and
    (SocketError = Src.SocketError);
end;

function TClientData.GetAccess(const Str: string): integer;
var
  I: integer;
begin
  Result := CI_ACCESS_READ;
  for I := 0 to ACCESS_COUNT-1 do
  if LowerCase(Str) = SAccess[I] then
  begin
    Result := AAccess[I];
    Exit;
  end;
end;

function TClientData.GetConnectionString: string;
var
  ConnectionString: TStrings;
begin
  ConnectionString := TStringList.Create;

  try
    ConnectionString.Delimiter := ';';
    ConnectionString.StrictDelimiter := True;

    ConnectionString.Values[IC_SERVER] := Server;
    if ConfigPort <> DEFAULT_CONFIG_PORT then ConnectionString.Values[IC_CONFIG_PORT] := ConfigPort;
    if ConnectInfo.Access <> DEFAULT_ACCESS then ConnectionString.Values[IC_ACCESS] := SAccess[ConnectInfo.Access];
    if DataPort <> DEFAULT_DATA_PORT then ConnectionString.Values[IC_DATA_PORT] := DataPort;
    if ConnectInfo.Timeout <> DEFAULT_TIMEOUT then ConnectionString.Values[IC_IDLE_TIMEOUT] := IntToStr(ConnectInfo.Timeout);
    ConnectionString.Values[IC_USER] := ConnectInfo.User;
    if Mode <> DEFAULT_MODE then ConnectionString.Values[IC_MODE] := Mode;

    if FDataBuffer.UseCompression <> DEFAULT_USE_COMPRESSION then ConnectionString.Add(IC_USE_COMPRESSION+ConnectionString.NameValueSeparator);
    if FDataBuffer.DeltaCount <> DEFAULT_DELTA_COUNT then ConnectionString.Values[IC_DELTA_COUNT] := IntToStr(FDataBuffer.DeltaCount);

    Result := ConnectionString.DelimitedText;
  finally
    ConnectionString.Free;
  end;
end;

function TClientData.LoadProperty(ATagId, ATypeId: integer; AFile: TIOBuffer): boolean;
begin
  Result := True;

  case ATagId of
    CPClientData_ConnectionString_Id:
      ConnectionString := AFile.ReadString;

    CPClientData_NetError_Id:
      NetChangerError := AFile.ReadInteger;

    CPClientData_ParamCount_Id:
      FParamCount := AFile.ReadInteger;

    CPClientData_SessionID_Id:
      SessionID := AFile.ReadInteger;

    CPClientData_SocketError_Id:
      SocketError := AFile.ReadInteger;
  else
    Result := inherited LoadProperty(ATagId, ATypeId, AFile);
  end;
end;

procedure TClientData.SaveProperty(AFile: TIOBuffer);
begin
  inherited;

  AFile.WriteProp(CPClientData_ConnectionString_Id);
  AFile.WriteType(CStringType_Id);
  AFile.WriteString(ConnectionString);

  AFile.WriteProp(CPClientData_NetError_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(NetChangerError);

  AFile.WriteProp(CPClientData_ParamCount_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(ParamCount);

  AFile.WriteProp(CPClientData_SessionID_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(SessionID);

  AFile.WriteProp(CPClientData_SocketError_Id);
  AFile.WriteType(CIntegerType_Id);
  AFile.WriteInteger(SocketError);
end;

procedure TClientData.SetConnectionString(const Value: string);
var
  ConnectionString: TStrings;

  function GetStringParam(const Key: string; const ADefault: string): string;
  var
    Str: string;
  begin
    Str := ConnectionString.Values[Key];
    if Str = '' then
      Result := ADefault
    else
      Result := Str;
  end;
  function GetIntParam(const Key: string; ADefault: integer): integer;
  var
    Str: string;
  begin
    Str := ConnectionString.Values[Key];
    if Str = '' then
      Exit(ADefault);

    try
      Result := StrToInt(Str);
    except
      Result := ADefault;
    end;
  end;
  function GetBoolParam(const Key: string; ADefault: boolean): boolean;
  begin
    if (ConnectionString.IndexOf(Key) > 0) or (ConnectionString.IndexOfName(Key) > 0) then
      Result := True
    else
      Result := ADefault;
  end;
begin
  ConnectionString := TStringList.Create;

  try
    ConnectionString.Delimiter := ';';
    ConnectionString.StrictDelimiter := True;
    ConnectionString.DelimitedText := Value;

    FConfigPort := GetStringParam(IC_CONFIG_PORT, DEFAULT_CONFIG_PORT);
    FDataPort := GetStringParam(IC_DATA_PORT, DEFAULT_DATA_PORT);
    FMode := GetStringParam(IC_MODE, DEFAULT_MODE);
    FServer := ConnectionString.Values[IC_SERVER];

    FConnectInfo.Access := GetAccess(ConnectionString.Values[IC_ACCESS]);
    FConnectInfo.Timeout := GetIntParam(IC_IDLE_TIMEOUT, DEFAULT_TIMEOUT);
    FConnectInfo.User := ConnectionString.Values[IC_USER];

    FDataBuffer.UseCompression := not GetBoolParam(IC_USE_COMPRESSION, not DEFAULT_USE_COMPRESSION);
    FDataBuffer.DeltaCount := GetIntParam(IC_DELTA_COUNT, DEFAULT_DELTA_COUNT);
  finally
    ConnectionString.Free;
  end;
end;

procedure TClientData.SetPassword(const APassword: string);
begin
  if APassword <> '' then
    AuthorizeInfo.Generate(ConnectInfo.User + APassword);
end;

procedure TClientData.WriteClass(AFile: TIOBuffer);
begin
  AFile.WriteClass(CTClientData_Id);
end;

{ TInfoNetClient }

constructor TInfoNetClient.Create(AData: TProvider; ATcpServer: TTcpServer<TClientData>; ATcpClient: TTcpClient<TClientData>);
begin
  inherited Create(AData, ATcpServer, ATcpClient);
  THashSetKeyCmd.Create(NC_EXTENDED_HASH_ENABLE_AND_SET_KEY, Parser);
  THashClearCmd.Create(NC_EXTENDED_HASH_CLEAR, Parser);
  TSetCypherKeyCmd.Create(NC_EXTENDED_SET_CYPHER_KEY, Parser);
end;

function TInfoNetClient.Query(ACmd: TPacketCommand; ARequest, AReply: TNetItem): integer;
var
  Index: integer;
  RequestPacket: TRequestPacket;
  ServerCmd: TServerCmd<TProvider, TClientData>;
begin
  RequestBuffer.NetPacket.Command := ACmd;
  RequestBuffer.NetPacket.LinkObject(ARequest);

  TcpClient.ClientData.FNetChangerError := 0;
  TcpClient.ClientData.FSocketError := 0;

  try
    if not Realize then Exit(NC_CANT_REALIZE_TRANSMITION);
    Result := ReplyBuffer.NetPacket.Status;

    if Result = NC_OK then
    begin
      if (AReply <> nil) and not AReply.LoadFromBuffer(ReplyBuffer.Buffer) then
          Exit(NC_TRANSMITION_ERROR)
    end;

    if ReplyBuffer.NetPacket.ExtendedCount <> 0 then
    begin
      RequestPacket := TRequestPacket.Create;

      try
        for Index := 0 to ReplyBuffer.NetPacket.ExtendedCount - 1 do
        begin
          if not TcpClient.Active then
            Exit;

          RequestPacket.LoadFromBuffer(ReplyBuffer.Buffer);
          ServerCmd := Parser.GetServerHandler(RequestPacket.Command);
          if ServerCmd = nil then
            Exit;

          ServerCmd.Execute(self);
        end;
      finally
        RequestPacket.Free;
      end;
    end;
  finally
    RequestBuffer.NetPacket.Release;
  end;
end;

{ THashSetKeyCmd }

procedure THashSetKeyCmd.Execute(ACommandServer: TCommandServer<TProvider, TClientData>);
var
  AcceptHash: TAcceptHash;
begin
  AcceptHash := TAcceptHash.Create;

  try
    if not AcceptHash.LoadFromBuffer(ACommandServer.ReplyBuffer.Buffer) then
      Exit;

    ACommandServer.ReplyBuffer.Hashed := True;
    ACommandServer.ReplyBuffer.NetHash.Base := AcceptHash.ReplyBase;
    ACommandServer.ReplyBuffer.NetHash.Interval := AcceptHash.ReplyInterval;

    ACommandServer.RequestBuffer.Hashed := True;
    ACommandServer.RequestBuffer.NetHash.Base := AcceptHash.RequestBase;
    ACommandServer.RequestBuffer.NetHash.Interval := AcceptHash.RequestInterval;
    ACommandServer.TcpClient.ClientData.AuthorizeInfo.Base := AcceptHash.PasswordBase;

  finally
    AcceptHash.Free;
  end;
end;

{ THashClearCmd }

procedure THashClearCmd.Execute(ACommandServer: TCommandServer<TProvider, TClientData>);
begin
  ACommandServer.ReplyBuffer.Hashed := False;
  ACommandServer.RequestBuffer.Hashed := False;
end;


{ TSetCypherKeyCmd }

procedure TSetCypherKeyCmd.Execute(ACommandServer: TCommandServer<TProvider, TClientData>);
var
  AcceptKey: TAcceptKey;
begin
  AcceptKey := TAcceptKey.Create;

  try
    if not AcceptKey.LoadFromBuffer(ACommandServer.ReplyBuffer.Buffer) then
      Exit;

    ACommandServer.ReplyBuffer.SetCryptoProvider(TNetCryptoProvider.Create(AcceptKey.ReplyKey));
    ACommandServer.RequestBuffer.SetCryptoProvider(TNetCryptoProvider.Create(AcceptKey.RequestKey));

  finally
    AcceptKey.Free;
  end;
end;

{ TInfoClient }

procedure TInfoClient.Close;
begin
  if TcpClient.Active then
    NetClient.Query(NC_CONNECTION_CLOSE, nil, nil);
end;

constructor TInfoClient.Create(AProvider: TProvider);
begin
  FProvider := AProvider;
  FTcpClient := TTcpClient<TClientData>.Create;
  FTcpClient.OnError := OnSocketError;
  FNetClient := TInfoNetClient.Create(FProvider, nil, FTcpClient);
  FNetClient.OnNetChangerError := OnNetChangerError;
  FProvider.AddClient(self);
end;

destructor TInfoClient.Destroy;
var
  Prov: TProvider;
begin
  if FProvider <> nil then
  begin
    Prov := FProvider;
    FProvider := nil;
    Prov.RemoveClient(self);
  end;

  Close;
  FNetClient.Free;
  FTcpClient.Free;
  inherited;
end;

procedure TInfoClient.OnNetChangerError(Sender: TObject; AError: Integer; AErrObj: TObject);
begin
  TcpClient.ClientData.NetChangerError := AError;
end;

procedure TInfoClient.OnSocketError(Sender: TObject; ASocketError: Integer);
begin
  TcpClient.ClientData.SocketError := ASocketError;
end;

function TInfoClient.Open(AConnectionString, APassword: PChar): integer;
var
  ConnectAccept: TConnectAcceptInfo;
begin
  TcpClient.ClientData.ConnectionString := AConnectionString;
  TcpClient.ClientData.SetPassword(APassword);

  TcpClient.RemoteHost := TSocketHost(TcpClient.ClientData.Server);
  if TcpClient.ClientData.Mode = IC_MODE_CONFIG then
    TcpClient.RemotePort := TSocketPort(TcpClient.ClientData.ConfigPort)
  else
    TcpClient.RemotePort := TSocketPort(TcpClient.ClientData.DataPort);

  TcpClient.Open;
  if not TcpClient.Connected then
    Exit(EC_OPENCONNECTION_ERROR);

  // set connection info
  TcpClient.ClientData.ConnectInfo.InterfaceInfo.RequestInterfaces;
  TcpClient.ClientData.ConnectInfo.SetAddress(TcpClient.GetLocalSocketAddress());

  // query to open connection
  ConnectAccept := TConnectAcceptInfo.Create;
  try
    NetClient.HandleBuffer;
    Result := NetClient.Query(NC_CONNECTION_OPEN, TcpClient.ClientData.ConnectInfo, ConnectAccept);
    if Result <> NC_OK then
    begin
      NetClient.RequestBuffer.Buffer.WriteToFile('dump.log');
      Exit;
    end;
    TcpClient.ClientData.SessionID := ConnectAccept.SessionID;
  finally
    ConnectAccept.Free;
  end;

  // query to authorize and open session
  TcpClient.ClientData.AuthorizeInfo.SetHashCode;
  Result := NetClient.Query(NC_CONNECTION_AUTHORIZE, TcpClient.ClientData.AuthorizeInfo, nil);
  if Result <> NC_OK then Exit;

  // Adjust transport layer
  if TcpClient.ClientData.Mode = IC_MODE_DATA then
    Result := NetClient.Query(NC_CONNECTION_HASH_DISABLE, nil, nil);
end;

{ TExtendedCmdThread }

constructor TExtendedCmdThread.Create(AInfoClient: TInfoClient);
begin
  inherited Create(True);
  FInfoClient := AInfoClient;
  FreeOnTerminate := True;
end;

procedure TExtendedCmdThread.Execute;
var
  ConnectionString: TStrings;
//  InfoClient: TInfoClient;
//  Res: integer;

//  Index: integer;
//  DatabaseName: string;
//  ParameterName: string;
begin
  ConnectionString := TStringList.Create;

  try
    ConnectionString.Delimiter := ';';
    ConnectionString.StrictDelimiter := True;

//    ConnectionString.DelimitedText := FInfoClient.Parameters.ConnectionString;
//    ConnectionString.Values[IC_IS_SERVER] := FReserveServerName;
//
//    InfoClient := TInfoClient.Create;
//    InfoClient.FInfoClientList := FInfoClient.FInfoClientList;
//    InfoClient.Parameters.ConnectionString := ConnectionString.DelimitedText;
  finally
    ConnectionString.Free;
  end;

//  if InfoClient <> nil then
//  begin
//    InfoClient.State := isOperate;
//
//    Res := InfoClient.Subscriber.Open;
//    if Res = EC_OK then
//    begin
//      for Index := 0 to FInfoClient.Subscriber.Params.Count - 1 do
//      begin
//        GetDatabaseAndParamName(FInfoClient.Subscriber.Params[Index], DatabaseName, ParameterName);
//
//        if InfoClient.Subscriber.CurrentDatabase <> DatabaseName then
//        begin
//          Res := InfoClient.Subscriber.RpcSelectDatabaseByName(DatabaseName);
//          if Res <> EC_OK then
//            Break;
//        end;
//
//        Res := InfoClient.Subscriber.RpcAddParameterByName(ParameterName);
//        if Res <> EC_OK then
//          Break;
//      end;
//    end;

//    if Res = EC_OK then
//    begin
//      if InfoClient.Parameters.Access and SBA_WRITE <> 0 then
//        Res := InfoClient.Subscriber.RpcReset(Now());
//
//      if InfoClient.Parameters.Access and SBA_READ <> 0 then
//        FInfoClient.FInfoClientList.TerminateClients;
//    end;
//
//    if Res = EC_OK then
//      FInfoClient.FInfoClientList.FItems.Add(InfoClient)
//    else
//      InfoClient.Free;
//  end;
end;

{ TClientExtendedCommand }

constructor TClientExtendedCommand.Create(AInfoClient: TInfoClient);
begin
  FInfoClient := AInfoClient;
//  FExtendedCmd := TInfoClient.Create(FInfoClient.TcpClient);
end;

//procedure TClientExtendedCommand.CreateReserveServer;
//var
//  ExtendedCmdThread: TExtendedCmdThread;
//begin
//  ExtendedCmdThread := TExtendedCmdThread.Create(FInfoClient);
//  ExtendedCmdThread.FReserveServerName := FExtendedCmd.InputBuffer.Handler.ReadString;
//  ExtendedCmdThread.Start;
//  Sleep(0);
//end;

destructor TClientExtendedCommand.Destroy;
begin
//  FExtendedCmd.Free;
  inherited;
end;

procedure TClientExtendedCommand.Execute;
//var
//  Extended: Cardinal;
begin
//  FExtendedCmd.OutputBuffer.Header.WriteCommand(NC_GET_EXTENDED_COMMAND);
//  FExtendedCmd.OutputBuffer.DataSize := 0;
//
//  if FExtendedCmd.Realize and (FExtendedCmd.InputBuffer.Header.ReadCommand = NC_OK) then
//  begin
//    FExtendedCmd.InputBuffer.Handler.Seek();
//
//    Extended := FExtendedCmd.InputBuffer.Handler.ReadInteger;
//
//    case Extended of
//      NS_SET_RESERVE_SERVER:
//        if ioGrantSplitDestinations in FInfoClient.Option then
//          CreateReserveServer;
//
//      NS_CLOSE_CONNECTION:
//        FInfoClient.Terminate;
//    end;
//  end;
end;

{ TProvider }

function TProvider.CreateClient: TInfoClient;
begin
  Result := TInfoClient.Create(self);
  Inc(FCurrentExtension);
  Result.TcpClient.ClientData.ConnectInfo.Extention := FCurrentExtension;
end;

procedure TProvider.AddClient(AClient: TInfoClient);
begin
  FItems.Add(AClient);
end;

procedure TProvider.ClearClients;
var
  Index: integer;
begin
  for Index := FItems.Count - 1 downto 0 do
    RemoveClient(FItems[Index]);
end;

constructor TProvider.Create;
begin
  FItems := TList<TInfoClient>.Create;
  FFreeOnCloseConnection := False;
  FCurrentExtension := -1;
end;

destructor TProvider.Destroy;
begin
  ClearClients;
  FItems.Free;
  inherited;
end;

function TProvider.GetClient(Index: integer): TInfoClient;
begin
  Result := FItems[Index];
end;

function TProvider.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TProvider.IndexOf(AClient: TInfoClient): integer;
begin
  Result := FItems.IndexOf(AClient);
end;

function TProvider.ReadParam(const ReadRec: TSubscriberReadRec9; Buffer: Pointer;
  var BufferSize: Cardinal): integer;
var
  Index: integer;
//  InfoClient: TInfoClient;
begin
  Result := EC_OK;
  Index := 0;

  while Index < FItems.Count do
  begin
//    InfoClient := FItems[Index];

//    if InfoClient.Terminated then InfoClient.Free
//    else
//    begin
//      if InfoClient.Parameters.Access = SBA_READ then
//      begin
//        Result := InfoClient.Subscriber.RpcReadParam9(ReadRec, Buffer, BufferSize);
//        Exit;
//      end;

      Inc(Index);
    end;
//  end;
end;

function TProvider.ReadParams(Buffer: Pointer; BufferSize: integer): integer;
var
  Index: integer;
//  InfoClient: TInfoClient;
begin
  Result := EC_OK;
  Index := 0;

  while Index < FItems.Count do
  begin
//    InfoClient := FItems[Index];

//    if InfoClient.Terminated then InfoClient.Free
//    else
//    begin
//      if InfoClient.Parameters.Access = SBA_READ then
//      begin
//        Result := InfoClient.Subscriber.RpcReadParams2(Buffer, BufferSize);
//        Exit;
//      end;

      Inc(Index);
//    end;
  end;
end;

procedure TProvider.RemoveClient(AClient: TInfoClient);
begin
  FItems.Remove(AClient);

  if AClient.FProvider <> nil then
  begin
    AClient.FProvider := nil;
    AClient.Free;
  end;

  if FreeOnCloseConnection and (FItems.Count = 0) then
    Free;
end;

function TProvider.Reset(const DateTime: TDateTime): integer;
var
  Index: integer;
//  InfoClient: TInfoClient;
begin
  Index := 0;
  Result := EC_OK;

  while Index < FItems.Count do
  begin
//    InfoClient := FItems[Index];

//    if InfoClient.Terminated then InfoClient.Free
//    else
//    begin
//      if InfoClient.Parameters.Access = SBA_WRITE then
//        Result := InfoClient.Subscriber.RpcReset(DateTime);
//
//      Inc(Index);
//    end;
  end;
end;

procedure TProvider.SetConnectionString(const AConnectionString: string);
var
  ConnectionParser: TStringList;
  I: integer;
//  InfoClient: TInfoClient;
  Str: string;
begin
//  ClearConnections;

  ConnectionParser := TStringList.Create;

  try
    ConnectionParser.Delimiter := ';';
    ConnectionParser.StrictDelimiter := True;
    ConnectionParser.DelimitedText := AConnectionString;

    // get server connection list
    Str := ConnectionParser.Values[IC_SERVER];

    if Str <> '' then
    begin
      ConnectionParser.Delete(ConnectionParser.IndexOfName(IC_SERVER));

      I := Pos(',', Str);

      while I > 0 do
      begin
//        InfoClient := CreateClient;
        ConnectionParser.Insert(0, IC_SERVER+'='+Copy(Str, 1, I-1));
//        InfoClient.Parameters.ConnectionString := ConnectionParser.Text;
        ConnectionParser.Delete(0);

        Str := Copy(Str, I+1, MaxInt);
        I := Pos(',', Str);
      end;

      // append last server connection
//      InfoClient := CreateClient;
      ConnectionParser.Insert(0, IC_SERVER+'='+Str);
//      InfoClient.Parameters.ConnectionString := ConnectionParser.DelimitedText;
    end;

  finally
    ConnectionParser.Free;
  end;
end;

procedure TProvider.TerminateClients;
var
  Index: integer;
//  InfoClient: TInfoClient;
begin
  for Index := 0 to FItems.Count - 1 do
  begin
//    InfoClient := FItems[Index];
//    InfoClient.Terminate;
  end;
end;

function TProvider.Write(Buffer: Pointer; BufferSize: integer): integer;
var
  Index: integer;
//  InfoClient: TInfoClient;
begin
  Index := 0;
  Result := EC_OK;

  while Index < FItems.Count do
  begin
//    InfoClient := FItems[Index];

//    if InfoClient.Terminated then InfoClient.Free
//    else
//    begin
//      if InfoClient.Parameters.Access = SBA_WRITE then
//        Result := InfoClient.Subscriber.RpcWrite(Buffer, BufferSize);
//
//      Inc(Index);
//    end;
  end;
end;

end.

