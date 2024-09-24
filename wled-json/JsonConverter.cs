namespace wled_json;

using System.Collections.Concurrent;
using System.Reflection;
using LanguageExt;
using LanguageExt.Traits;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

public class OptionJsonConverter : JsonConverter
{
    private static readonly ConcurrentDictionary<Type, ReflectionTypeData> cachedReflection =
        new ConcurrentDictionary<Type, ReflectionTypeData>();

    private ReflectionTypeData GetForOptionType(Type optionType)
    {
        return cachedReflection.GetOrAdd(optionType.GetGenericArguments().Last(),
            t => new ReflectionTypeData(optionType));
    }

    public override bool CanConvert(Type objectType)
    {
        return objectType.IsGenericType && (
            objectType.GetGenericTypeDefinition().IsAssignableFrom(typeof(Option<>)) || 
            objectType.GetGenericTypeDefinition().IsAssignableFrom(typeof(K<,>)));
    }

    public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
    {
        ReflectionTypeData typeData = GetForOptionType(value.GetType());

        if ((bool)typeData.IsNoneProp.GetValue(value))
        {
            writer.WriteNull();
            return;
        }

        serializer.Serialize(writer, typeData.IfNoneMethod.Invoke(value, new object[] { null }));
    }

    public override object ReadJson(JsonReader reader, Type objectType, object existingValue,
        JsonSerializer serializer)
    {
        ReflectionTypeData typeData = GetForOptionType(objectType);

        if (reader.TokenType == JsonToken.Null)
        {
            return typeData.NoneField.GetValue(null);
        }

        object result = serializer.Deserialize(reader, objectType.GetGenericArguments().Last());
        return typeData.SomeMethod.Invoke(null, new[] { result });
    }

    private class ReflectionTypeData
    {
        public ReflectionTypeData(Type optionType)
        {
            if(optionType.IsGenericType && optionType.GetGenericTypeDefinition().IsAssignableFrom(typeof(K<,>))) {
                optionType = typeof(Option<>).MakeGenericType(optionType.GetGenericArguments().Last());
            }
            IsNoneProp = optionType.GetProperty(nameof(Option<object>.IsNone), BindingFlags.Instance | BindingFlags.Public);
            IfNoneMethod = optionType.GetMethods(BindingFlags.Instance | BindingFlags.Public).Where(m => m.Name == nameof(Option<object>.IfNone) && m.GetParameters().Length == 1 && m.GetParameters()[0].ParameterType.IsGenericType).FirstOrDefault();
            NoneField = optionType.GetField(nameof(Option<object>.None), BindingFlags.Static | BindingFlags.Public);
            SomeMethod = optionType.GetMethod(nameof(Option<object>.Some), BindingFlags.Static | BindingFlags.Public);
        }

        public PropertyInfo IsNoneProp { get; }
        public MethodInfo IfNoneMethod { get; }
        public FieldInfo NoneField { get; }
        public MethodInfo SomeMethod { get; }
    }
}

public class IdentityJsonConverter : JsonConverter
{
    public override bool CanConvert(Type objectType)
    {
        return objectType.IsGenericType && (
            objectType.GetGenericTypeDefinition().IsAssignableFrom(typeof(Identity<>)) || 
            objectType.GetGenericTypeDefinition().IsAssignableFrom(typeof(K<,>)));
    }

    public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
    {
        var identityType = typeof(Identity<>).MakeGenericType(value.GetType().GetGenericArguments().Last());
        var valueProp = identityType.GetProperty(nameof(Identity<object>.Value), BindingFlags.Instance | BindingFlags.Public);

        var x = valueProp.GetValue(value);

        serializer.Serialize(writer, x);
    }

    public override object ReadJson(JsonReader reader, Type objectType, object existingValue,
        JsonSerializer serializer)
    {
        object result = serializer.Deserialize(reader, objectType.GetGenericArguments().Last());
        var identityType = typeof(Identity<>).MakeGenericType(result.GetType());
        var ctor = identityType.GetMethod(nameof(Identity<object>.Pure), BindingFlags.Static | BindingFlags.Public);
        return ctor.Invoke(null, new[] { result });
    }
}

public class ShouldSerializeContractResolver : DefaultContractResolver
{
    public new static readonly ShouldSerializeContractResolver Instance = new ShouldSerializeContractResolver();

    protected override JsonProperty CreateProperty(MemberInfo member, MemberSerialization memberSerialization)
    {
        JsonProperty property = base.CreateProperty(member, memberSerialization);

        property.ShouldSerialize = instance => !instance.Equals(Option<bool>.None) && !instance.Equals(Option<int>.None);
        return property;
    }
}