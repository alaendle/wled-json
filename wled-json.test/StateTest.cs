namespace wled_json.test;

using Xunit;
using FsCheck.Xunit;
using LanguageExt;
using LanguageExt.Traits;
using FsCheck;
using Newtonsoft.Json;
using Xunit.Abstractions;

public class LampStateTest(ITestOutputHelper output)
{
    private readonly ITestOutputHelper output = output;

    [Fact]
    public void Test1()
    {
        var patch = new wled_json.State<Option> { On = Option<bool>.Some(false), Bri = Option<int>.None, Transition = Option<int>.Some(193), Ps = Option<int>.Some(209), Pl = Option<int>.None, Lor = Option<int>.Some(254), Mainseg = Option<int>.Some(5) };
        var jsonPatch = JsonConvert.SerializeObject(patch, new JsonSerializerSettings { Converters = [new wled_json.OptionJsonConverter()], ContractResolver = ShouldSerializeContractResolver.Instance });
        output.WriteLine(jsonPatch);

        var patch_ = wled_json.State.Empty<Option>();
        JsonConvert.PopulateObject(jsonPatch, patch_, new JsonSerializerSettings { Converters = [new wled_json.OptionJsonConverter() ] });
        output.WriteLine(patch_.ToString());
        output.WriteLine(patch.ToString());

        output.WriteLine(patch.Equals(patch_).ToString());
    }

    [Fact]
    public void TestRequiredAttribute() {
        Console.WriteLine(typeof(wled_json.State<int>).GetConstructor(Array.Empty<Type>()).Invoke(Array.Empty<object>()));
    }
    
    
    [Property(Arbitrary = [typeof(OptionalBoolGenerator), typeof(OptionalIntGenerator)])]
    bool TestSerializationRoundTripForOption(wled_json.State<Option> x)
    {
        var json = JsonConvert.SerializeObject(x, new JsonSerializerSettings { Converters = [new wled_json.OptionJsonConverter()], ContractResolver = ShouldSerializeContractResolver.Instance });
        var x_ = wled_json.State.Empty<Option>();
        JsonConvert.PopulateObject(json, x_, new JsonSerializerSettings { Converters = [new wled_json.OptionJsonConverter()] });
        return x.Equals(x_);
    }

    [Property(Arbitrary = [typeof(IdentityBoolGenerator), typeof(IdentityIntGenerator)])]
    bool TestSerializationRoundTripForIdentity(wled_json.State<Identity> x)
    {
        var json = JsonConvert.SerializeObject(x, new JsonSerializerSettings { Converters = [new wled_json.IdentityJsonConverter()] });
        output.WriteLine(json);
        var x_ = JsonConvert.DeserializeObject<wled_json.State<Identity>>(json, new JsonSerializerSettings { Converters = [new wled_json.IdentityJsonConverter()] });
        output.WriteLine(x_.ToString());
        return x.Equals(x_);
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

public static class IdentityBoolGenerator
{
    public static Arbitrary<K<Identity, bool>> Generate() =>
      Gen.Elements(new[] { (K<Identity, bool>)Identity<bool>.Pure(true), Identity<bool>.Pure(false) }).ToArbitrary();
}

public static class IdentityIntGenerator
{
    public static Arbitrary<K<Identity, int>> Generate() =>
      Gen.Choose(0,255).Select(x => (K<Identity, int>)Identity<int>.Pure(x)).ToArbitrary();
}
