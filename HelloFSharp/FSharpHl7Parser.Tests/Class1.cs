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

            var secSeg = result.segments.Last();
            foreach (var f in secSeg.fields)
            {
                Console.WriteLine(f.GetField().ToString());
            }

            Console.WriteLine(secSeg.fields.ToString());
            Console.WriteLine(secSeg.ToString());

            Assert.AreEqual("PID", secSeg.name);
            Assert.AreEqual(3, secSeg.fields.Length);
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
