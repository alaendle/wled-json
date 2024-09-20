namespace wled_json.test;

using Xunit;
using FsCheck.Xunit;
using LanguageExt;
using LanguageExt.Traits;
using FsCheck;

public class LampStateTest
{
    [Fact]
    public void Test1()
    {
        Console.WriteLine("Hello World!");
    }

    [Property(Arbitrary = [typeof(OptionalBoolGenerator), typeof(OptionalIntGenerator)])]
    public bool TestAssociativity(wled_json.State<Option> x, wled_json.State<Option> y, wled_json.State<Option> z)
    {
        // proof: x <> (y <> z) == (x <> y) <> z
        return x.Combine(y.Combine(z)).Equals(x.Combine(y).Combine(z));
    }

    [Property(Arbitrary = [typeof(OptionalBoolGenerator), typeof(OptionalIntGenerator)])]
    public bool TestMonoid(wled_json.State<Option> x)
    {
        // proof: mempty <> x = x  -- and --  x <> mempty = x
        return wled_json.State.Empty<Option>().Combine(x).Equals(x) && 
               x.Combine(wled_json.State.Empty<Option>()).Equals(x);
    }
}

public static class OptionalBoolGenerator
{
    public static Arbitrary<K<Option, bool>> Generate() =>
      Gen.Elements(new[] { (K<Option, bool>)Option<bool>.None, Option<bool>.Some(true), Option<bool>.Some(false) }).ToArbitrary();
}

public static class OptionalIntGenerator
{
    public static Arbitrary<K<Option, int>> Generate() =>
      Gen.Choose(-255,255).Select(x => x < 0 ? (K<Option, int>)Option<int>.None : Option<int>.Some(x)).ToArbitrary();
}
