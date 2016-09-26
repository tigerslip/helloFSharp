using System;
using System.Diagnostics;
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

            var pid = result.segments.Last();

            Assert.AreEqual("PID", pid.name);
            Assert.AreEqual(3, pid.fields.Length);
            Assert.AreEqual("A", pid.fields.First().GetField().ToString());
            Assert.AreEqual("B", pid.fields.ElementAt(1).GetField().ToString());
            Assert.AreEqual("C", pid.fields.ElementAt(2).GetField().ToString());
        }

        [Test]
        public void SegmentToString()
        {
            var msh = "MSH|~^&\\|A|B|C";
            var pid = "PID|A|B|C";
            var hl7 = $"{msh}\r\n{pid}";
            var result = Hl7Parser.Parse(hl7);
            Assert.AreEqual(pid, result.segments.Last().ToString());
            Assert.AreEqual(msh, result.segments.First().ToString());
        }

        [Test]
        public void MshFieldsAreInCorrectPosition_AccountingForWeirdMshRule()
        {
            var result = Hl7Parser.Parse("MSH|~^&\\|A|B|C");
            var seg = result.segments.Single();
            var a = seg.fields.First();

            Assert.AreEqual("|", a.GetField().ToString());
            Assert.AreEqual(5, seg.fields.Length);
        }
    }

    public static class FieldOrRepExtensions
    {
        public static Hl7Parser.Field GetField(this Hl7Parser.FieldOrRepetitions field)
        {
            return ((Hl7Parser.FieldOrRepetitions.Field) field).Item;
        }

        public static Hl7Parser.Repetitions GetRepetition(this Hl7Parser.FieldOrRepetitions repetition)
        {
            return ((Hl7Parser.FieldOrRepetitions.Repetitions)repetition).Item;
        }
    }
}
