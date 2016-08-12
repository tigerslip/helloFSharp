using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using HelloFSharpLib;

namespace FSharpInterop
{
    public class Tests
    {
        [Test]
        public void CanWe()
        {
            var interop = new Class1();
            var result = interop.X;

            Assert.AreEqual("F#", result);

            Assert.AreEqual(4, interop.square(2));
        }

        public class Thing
        {
            public string Name { get; set; }
        }
    }
}
