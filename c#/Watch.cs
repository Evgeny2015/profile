using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Stgraph.Models
{
    public class Appoint
    {
        public class Request
        {
            public int sid;
            public DateTime date;
        }

        public class Post
        {
            public int id;
            public string name;
        }

        public class Shop
        {
            public int id;
            public string name;
        }

        public class Watch
        {
            public int id;
            public string name;
        }

        public class App
        {
            public int id;
            public int wid;
            public int pid;
            public string fam;
        }

        //public Request request;
        public List<Post> post;
        public List<Watch> watch;
        public List<App> app;
    }

    // Персонал
    public class Staff
    {
        [Key]
        public int Id;

        [Required]
        //[DataType(DataType.)]
        [HiddenInput(DisplayValue = false)]
        public int ShopId { get; set; }

        [Required]        
        [DisplayName("Фамилия")]
        [DisplayFormat(NullDisplayText = "Фамилия")]
        public string First { get; set; }

        [Required]
        [DisplayName("Имя")]
        public string Second { get; set; }

        [Required]
        [DisplayName("Отчество")]
        public string Last { get; set; }

        public Staff() { }
        public Staff(int id, int shopId, string first, string sec, string last)
        {
            Id = id;
            ShopId = shopId;
            First = first;
            Second = sec;
            Last = last;
        }
        public Staff(int shopId, string first, string sec, string last)
        {
            ShopId = shopId;
            First = first;
            Second = sec;
            Last = last;
        }
    }
}