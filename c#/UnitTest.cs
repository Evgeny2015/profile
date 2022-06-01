using System;
using System.IO;
using System.Xml;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using StgraphDB;
using StgraphDbService;
using StgraphDbTest.StgraphDBServiceReference;

namespace StgraphDbTest
{
    [TestClass]
    public class UnitTest
    {
        [TestMethod]
        public void TestLibrary()
        {
            using (FileStream fs = new FileStream(@"Tb0008.dat", FileMode.Open))
            {
                InstantData id = new InstantData(fs);

                Assert.AreEqual(id.Analog[0], 5);
                Assert.AreEqual(id.GetDiscret(1, DiscretType.MechThree), DiscretState.Error);
                Assert.AreEqual(id.GetDiscret(3, DiscretType.MechTwo), DiscretState.Open);
            }
        }

        [TestMethod]
        public void TestLocalService()
        {
            using (FileStream fs = new FileStream(@"StgraphDB.xml", FileMode.Open))
            {
                var database = Utils.StreamToObject<Database>(fs);
                var d = database.Discret[1];

                Assert.AreEqual((DiscretType)d.Type, DiscretType.MechThree);
            }
        }
    }

    [TestClass]
    public class ServiceTest
    {
        [TestMethod]
        public void GetDiscretCount()
        {
            var client = new DbServiceClient();

            Assert.AreEqual(client.GetDiscretCount(), Globals.DISCRET_COUNT);

            client.Close();
        }

        [TestMethod]
        public void GetSchema()
        {
            var client = new DbServiceClient();
            var stream = client.GetSchema();

            XmlDocument doc = new XmlDocument();
            doc.Load(stream);

            var root = doc.DocumentElement;

            Assert.AreEqual(root.ChildNodes.Count, 2);

            client.Close();
        }

        [TestMethod]
        public void GetDiscrets()
        {
            var client = new DbServiceClient();
            var data = client.GetDiscrets(Complex.Block8);
            client.Close();

            Assert.AreEqual(data.Length, 64);
        }

        [TestMethod]
        public void GetDateDiscrets()
        {
            var client = new DbServiceClient();
            var data = client.GetDateDiscrets(Complex.Block8);
            client.Close();

            Assert.AreEqual(data.Data.Length, 64);
        }
    }
}
