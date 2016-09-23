using NUnit.Framework;

namespace FSharpHl7Parser.Tests
{
    public class Class1
    {
        [Test]
        public void InteropTests()
        {
            var parser = new Hl7Parser.Hl7Parser();
            var result = parser.Parse("MSH|~^&\\|A|B|C\r\nPID|A|B|C");
            Assert.AreEqual(2, result.segments.Length);
        }
    }
}
