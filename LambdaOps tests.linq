<Query Kind="Statements">
  <Reference Relative="lambdaOps.dll">C:\projects\Fsi\lambdaOps.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

Func<string,int,bool> testMethod = (s,i) => string.IsNullOrEmpty(s) && i!=0;

var expected1 = testMethod("",1);
var curried1 = LambdaOps.LambdaOp.Curry(testMethod);

var actual1 = curried1("")(1);
new{expected1,actual1}.Dump();

Func<string,int,bool,string> test2Method = (s,i,b) => new{ s,i,b}.GetHashCode().ToString();

var expected2 = test2Method("hello",5,true);
var actual2 = LambdaOps.LambdaOp.Curry(test2Method)("hello")(5)(true);
new {expected2, actual2}.Dump();

var actual1a = LambdaOps.LambdaOp.Apply(testMethod,"")(1);

new {expected1,actual1a}.Dump();
