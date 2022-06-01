using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Web;
using System.Web.Http;

namespace DocApplication.Models
{
    public class DownloadFile : IHttpActionResult
    {
        public Document Doc { get; private set; }

        public DownloadFile (Document doc)
        {
            this.Doc = doc;
        }
        public Task<HttpResponseMessage> ExecuteAsync(CancellationToken cancellationToken)
        {
            var path = Service.Service.GetRemoteFileName(Doc, false);
            var response = new HttpResponseMessage
            {
                //Content = new StreamContent(File.OpenRead(path))
                Content = new StreamContent(new FileStream(path, FileMode.Open, FileAccess.Read))
            };
            response.Content.Headers.ContentType = new MediaTypeHeaderValue("application/octet-stream");
            response.Content.Headers.ContentDisposition = new ContentDispositionHeaderValue("attachment")
            {
                FileName = Doc.OriginalName,
                FileNameStar = Doc.OriginalName
            };

            return Task.FromResult(response);
        }
    }
}