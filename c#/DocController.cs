using DocApplication.Models;
using DocApplication.Service;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;
using System.Web;
using System.Web.Http;

namespace DocApplication.Controllers
{
    /// <summary>
    /// Doc controller
    /// </summary>
    public class DocController : ApiController
    {
        private readonly IDocumentService docService;        
        public DocController ()
        {
            docService = new DocumentService();        
        }
        /// <summary>
        /// Get doc list for date and tag list
        /// </summary>
        /// <param name="date">Request date</param>
        /// <param name="tags">Tag list</param>
        /// <param name="page">Optional page number</param>
        /// <returns></returns>
        [HttpGet]        
        [Route("api/doc/getDocs/{date}/{tags}/{page?}")]
        public async Task<IHttpActionResult> GetDocs(DateTime date, string tags, int page = 0 ) {
            var tagList = tags.Split(';');
            var res = new Dictionary<int, DocumentRequest>();
            
            foreach (var tag in tagList) {                 
                foreach (var x in await docService.GetDocsAsync(date, tag, page, 10))
                {
                    if (!res.ContainsKey(x.DocId))
                        res.Add(x.DocId, x);
                }
            }
            return Json(new
            {
                docs = from x in res.Values
                       select new { id = x.DocId, fileName = x.FileName }
            });
        }
        [HttpGet]
        [Route("api/doc/download/{id}")]
        public async Task<IHttpActionResult> DownloadFile (int id)
        {
            // DOCUMENTSERVICE: Get document
            var doc = await docService.GetDocAsync(id);
            if (doc == null)
                return NotFound();

            #if DEBUG
            doc.FileName = "a";
            #endif            
            
            return new DownloadFile(doc);
        }
        [HttpPost]
        [Route("api/doc/RemoveFile/{sessionName}/{docId}")]
        public async Task<IHttpActionResult> RemoveFile(string sessionName, int docId)
        {
            try
            {
                // DOCUMENTSERVICE: Open connection
                await docService.OpenConnectionAsync();

                // DOCUMENTSERVICE: Get session id
                var sessionId = await docService.GetSessionAsync(sessionName, Request.Headers.Host);

                // DOCUMENTSERVICE: Get document
                var doc = await docService.GetDocAsync(docId);
                var sessLink = await docService.GetSessionLinkAsync(sessionId, docId);

                if (!sessLink.State.Equals(DocumentState.FileLinked))
                    File.Delete(Service.Service.GetRemoteFileName(doc, false));

                await docService.RemoveFile(sessionId, doc.DocId);

            }
            catch (Exception e)
            {
                return BadRequest(e.ToString());
            }
            finally
            {
                docService.CloseConnection();
            }
            
            return Ok();
        }
        [HttpPost]
        [Route("api/doc/upload/{sessionName}")]
        public async Task<IHttpActionResult> UploadFile (string sessionName) {

            var context = HttpContext.Current;
            var root = context.Server.MapPath("~/App_Data");            
            var provider = new MultipartFormDataStreamProvider(root);                        
            var docList = new List<DocumentRespond>();

            try
            {
                // Get files
                await Request.Content.ReadAsMultipartAsync(provider);

                // DOCUMENTSERVICE: Open connection
                await docService.OpenConnectionAsync();

                // DOCUMENTSERVICE: Get session id
                var sessionId = await docService.GetSessionAsync(sessionName, Request.Headers.Host);

                // Add files
                foreach (var x in provider.FileData)
                {
                    var fileName = Path.GetFileName(x.Headers.ContentDisposition.FileName.Trim('"'));
                    //var fileSize = x.Headers.ContentDisposition.Size;                    

                    // Get file hash
                    var hash = Service.Service.GetHash(x.LocalFileName);

                    // DOCUMENTSERVICE: Add file 
                    var doc = await docService.AddFileAsync(sessionId, fileName, hash);                    
                    docList.Add(new DocumentRespond { 
                        DocId = doc.DocId,
                        FileName = doc.OriginalName,
                        State = doc.State
                    });

                    // If file already exists remove local file
                    if (doc.State.Equals(DocumentState.FileLinked))                    
                    {
                        File.Delete(x.LocalFileName);
                    }

                    // Move local file to remote catalog
                    else
                    {                        
                        File.Move(x.LocalFileName, Service.Service.GetRemoteFileName(doc, true));

                        // DOCUMENTSERVICE: Mark file as copied
                        await docService.SetStateAsync(sessionId, doc.DocId, DocumentState.FileCopied);
                    }
                }
            }
            catch (Exception e)
            {
                return BadRequest(e.ToString());
            }
            finally
            {
                docService.CloseConnection();
            }
            
            return Ok(new { docList });
        }
        [HttpPost]
        [Route("api/doc/SetTag/{sessionName}")]
        public async Task<IHttpActionResult> SetTag (string sessionName, [FromBody] SetTagRequest tagRequest)//
        {
            var tag = tagRequest.Tags
                .Split(';')
                .Where(x => x.Trim() != string.Empty)
                .Select(x => x.Trim())
                .ToList();

            try
            {
                // DOCUMENTSERVICE: open connection
                await docService.OpenConnectionAsync();

                // DOCUMENTSERVICE: Get session ID
                var sessionId = await docService.GetSessionAsync(sessionName, Request.Headers.Host);

                // DOCUMENTSERVICE: write tags
                foreach (var x in tag)
                    await docService.AddTagAsync(sessionId, x);

                // DOCUMENTSERVICE: close session
                await docService.CloseSessionAsync(sessionId);
            }
            catch (Exception e)
            {
                return BadRequest(e.ToString());
            }
            finally
            {
                docService.CloseConnection();
            }

            return Ok();
        }
    }
}
