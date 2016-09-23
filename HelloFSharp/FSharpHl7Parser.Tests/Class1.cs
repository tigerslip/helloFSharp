using System.Linq;
using NUnit.Framework;

namespace FSharpHl7Parser.Tests
{
    public class Class1
    {
        [Test]
        public void InteropTests()
        {
            var result = Hl7Parser.Parse("MSH|~^&\\|A|B|C\r\nPID|A|B|C");
            Assert.AreEqual(2, result.segments.Length);
            var firstSeg = result.segments.First();
            Assert.AreEqual("MSH", firstSeg.name);
        }

        [Test]
        public void MshFieldsAreInCorrectPosition_AccountingForWeirdMshRule()
        {
            var result = Hl7Parser.Parse("MSH|~^&\\|A|B|C");
            var seg = result.segments.Single();
            var seperator = seg.fields.ElementAt(1);
            Assert.AreEqual(6, result.segments.Length);
        }
    }
}
