using Dapper;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Threading.Tasks;
using System.Web;

namespace DocApplication.Models
{
    public class DocumentService : Service.IDocumentService
    {
        private readonly string connectionString = ConfigurationManager.ConnectionStrings["DefaultConnection"].ConnectionString;
        private readonly SqlConnection connection;

        public DocumentService()
        {
            connection = new SqlConnection(connectionString);
        }
        protected async Task<List<T>> GetDataAsync<T>(CommandDefinition command)
        {
            var res = new List<T>();
            using (var reader = await connection.ExecuteReaderAsync(command))
            {
                var dt = reader.GetSchemaTable();
                var r = dt.Rows.Cast<T>();
                
                //while (reader.Read())
                    //res.Add(reader.Ge(0));
            }
            return res;
        }
        public async Task<DocumentNew> AddFileAsync(int sessionId, string fileName, byte[] hash)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessId", sessionId, DbType.Int32, ParameterDirection.Input);
            param.Add("originalName", fileName, DbType.String, ParameterDirection.Input);
            param.Add("hash", hash, DbType.Binary, ParameterDirection.Input);

            // query dataset
            return (await connection
                .QueryAsync<DocumentNew>("document.AddFile", param, commandType: CommandType.StoredProcedure))
                .FirstOrDefault();
        }
        public async Task AddTagAsync(int sessionId, string tag)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessId", sessionId, DbType.Int32, ParameterDirection.Input);
            param.Add("tag", tag, DbType.String, ParameterDirection.Input);

            await connection.ExecuteAsync("document.AddTag", param, commandType: CommandType.StoredProcedure);

            return;
        }
        public void CloseConnection()
        {
            connection.Close();
        }
        public async Task CloseSessionAsync(int sessionId)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessId", sessionId, DbType.Int32, ParameterDirection.Input);                

            await connection.ExecuteAsync("document.CloseSession", param, commandType: CommandType.StoredProcedure);

            return;
        }
        public async Task<Document> GetDocAsync(int docId)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("docId", docId, DbType.Int32, ParameterDirection.Input);

            // query dataset
            return (await connection
                .QueryAsync<Document>("document.GetDoc", param, commandType: CommandType.StoredProcedure))
                .FirstOrDefault();
        }
        public async Task<IEnumerable<DocumentRequest>> GetDocsAsync(DateTime date, string tag, int page, int pageSize)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("date", date, DbType.DateTime, ParameterDirection.Input);
            param.Add("tag", tag, DbType.String, ParameterDirection.Input);
            param.Add("page_number", page, DbType.Int32, ParameterDirection.Input);
            param.Add("page_size", pageSize, DbType.Int32, ParameterDirection.Input);

            // query dataset
            return (await connection
                .QueryAsync<DocumentRequest>("document.GetDocs", param, commandType: CommandType.StoredProcedure));
        }
        public async Task<int> GetSessionAsync(string session, string host)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessionName", session, DbType.String, direction: ParameterDirection.Input);
            param.Add("host", host, DbType.String, direction: ParameterDirection.Input);
            param.Add("result", dbType: DbType.Int32, direction: ParameterDirection.ReturnValue);

            await connection.ExecuteAsync("document.GetSession", param, commandType: CommandType.StoredProcedure);

            return param.Get<int>("result");
        }
        public async Task OpenConnectionAsync()
        {
            await connection.OpenAsync();
            return;
        }
        public async Task<int> RemoveFile(int sessionId, int docId)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessId", sessionId, DbType.Int32, ParameterDirection.Input);
            param.Add("docId", docId, DbType.Int32, ParameterDirection.Input);
            param.Add("result", dbType: DbType.Int32, direction: ParameterDirection.ReturnValue);

            // query dataset
            await connection.ExecuteAsync("document.RemoveFile", param, commandType: CommandType.StoredProcedure);

            return param.Get<int>("result"); ;
        }
        public async Task<int> SetStateAsync(int sessionId, int docId, DocumentState state)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessId", sessionId, DbType.Int32, ParameterDirection.Input);
            param.Add("docId", docId, DbType.Int32, ParameterDirection.Input);
            param.Add("state", state, DbType.Int32, ParameterDirection.Input);
            param.Add("result", dbType: DbType.Int32, direction: ParameterDirection.ReturnValue);


            // query dataset
            await connection.ExecuteAsync("document.SetState", param, commandType: CommandType.StoredProcedure);

            return param.Get<int>("result");
        }

        public async Task<SessionLink> GetSessionLinkAsync (int sessionId, int docId)
        {
            // set parameter
            var param = new DynamicParameters();
            param.Add("sessionId", sessionId, DbType.Int32, ParameterDirection.Input);
            param.Add("docId", docId, DbType.Int32, ParameterDirection.Input);

            // query dataset
            return (await connection
                .QueryAsync<SessionLink>("document.GetState", param, commandType: CommandType.StoredProcedure))
                .FirstOrDefault();
        }
    }
}