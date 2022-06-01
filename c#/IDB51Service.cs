using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.ServiceModel;

namespace DB51WcfService
{
    [ServiceContract]
    public interface IDB51Service
    {
        [OperationContract]
        ArchiveResponse GetArchive(ArchiveRequest request);

        [OperationContract]
        InfoResponse GetInfo();

        [OperationContract]
        KksResponse GetKksSignal(string kks);
    }

    [DataContract]
    public class Param
    {
        [DataMember]
        public string Kks;

        [DataMember]
        public string Signal;
    }

    [DataContract]
    public class ArchiveRequest
    {
        [DataMember]
        public Param Param;

        [DataMember]
        public DateTime FromDate;

        [DataMember]
        public DateTime ToDate;
    }

    [DataContract]
    public class ArchiveItem
    {
        [DataMember]
        public DateTime Date;

        [DataMember]
        public double Value;

        [DataMember]
        public int Quality;

        public ArchiveItem Clone()
        {
            return new ArchiveItem()
            {
                Date = this.Date,
                Value = this.Value,
                Quality = this.Quality
            };
        }
    }

    [DataContract]
    public enum SignalType : int {[EnumMember] Unknown, [EnumMember] Analog, [EnumMember] Discret }

    [DataContract]
    public enum ServiceResult : int {
        [EnumMember] Ok,
        [EnumMember] DbConnectionError,
        [EnumMember] DbExecError,
        [EnumMember] DateRequestError,
        [EnumMember] FileNotFound,
        [EnumMember] FileError }

    [DataContract]
    public class ArchiveResult
    {
        [DataMember]
        public string Kks;
        [DataMember]
        public string Signal;
        [DataMember]
        public SignalType SignalType;
        [DataMember]
        public List<ArchiveItem> Data;
    }
    [DataContract]
    public class ArchiveResponse
    {
        [DataMember]
        public ServiceResult Result;
        [DataMember]
        public ArchiveResult Archive;

        public ArchiveResponse(ServiceResult result)
        {
            Result = result;
        }
    }

    [DataContract]
    public class Info
    {
        [DataMember]
        public int PageSize;
        [DataMember]
        public DateTime MinDate;
        [DataMember]
        public DateTime MaxDate;
    }
    [DataContract]
    public class InfoResponse
    {
        [DataMember]
        public ServiceResult Result;
        [DataMember]
        public Info Info;

        public InfoResponse(ServiceResult result)
        {
            Result = result;
        }
    }
    [DataContract]
        
    public class KksResponse
    {
        [DataMember]
        public ServiceResult Result;
        [DataMember]
        public List<Param> List;

        public KksResponse(ServiceResult result)
        {
            Result = result;
        }
    }

}
