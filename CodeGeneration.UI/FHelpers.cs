using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace CodeGeneration.UI
{
    public static class FHelpers
    {
        public static FSharpMap<TKey, TValue> ToMap<TKey, TValue>(this IEnumerable<Tuple<TKey, TValue>> items)
             => new FSharpMap<TKey, TValue>(items);
        public static bool IsValueString(this string x)
            => BReusable.StringPatterns.isValueString.Invoke(x);
    }

}
