unit Net.Int;

interface

type
  IAggrigateObject = interface;
  ISecurityObject = interface
    ['{65D4BBA8-47C0-401B-9C05-4F591C32B986}']
    function GetLockObject: ISecurityObject;
    function GetObjectName: string;
    function GetOwner: IAggrigateObject;
    property LockObject: ISecurityObject read GetLockObject;
    property ObjectName: string read GetObjectName;
    property Owner: IAggrigateObject read GetOwner;
  end;

  IManagedObject = interface(ISecurityObject)
    ['{D5098B9E-5E04-4DB3-A02F-FBD084805F95}']
    function GetEnabled: boolean;
    property Enabled: boolean read GetEnabled;
  end;

  IAggrigateObject = interface(IManagedObject)
    ['{982B3019-88F6-4FB8-BD84-58F610702133}']
    function AddObject(AObj: ISecurityObject): integer;
    function GetCount: integer;
    function GetObject(const AObjectName: string): ISecurityObject;
    function RemoveObject(AObj: ISecurityObject): integer;
    property Count: integer read GetCount;
  end;

  ICollectionObject = interface(IAggrigateObject)
    ['{C193EAD7-B876-4066-927A-8458E96F5EDB}']
    function GetItem(Index: integer): ISecurityObject;
    property Items[Index: integer]: ISecurityObject read GetItem;
  end;

  IDictionaryObject = interface(IAggrigateObject)
    ['{1FA668F6-699C-41C5-9A3C-4B97387DB3AC}']
  end;

  IKeyCollectionObject = interface(ICollectionObject)
    ['{6DDFE25F-04C2-4E1C-9DDC-6AE95AFA5843}']
  end;

  IDatabaseParam = interface(ISecurityObject)
    ['{9F55A3C0-2941-45D1-8D37-ACA4CB87FED4}']
  end;

  IDatabase = interface(ICollectionObject)
    ['{94ED2829-C443-4346-A660-FC6BCC716B6D}']
  end;

implementation

end.
