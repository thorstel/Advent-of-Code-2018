using Microsoft.Z3;
using System;
using System.IO;
using System.Text.RegularExpressions;

namespace AoC2018_Day23
{
    public class Program
    {
        static ArithExpr GetAbsExpr(Context ctx, ArithExpr expr)
        {
            return (ArithExpr)ctx.MkITE(ctx.MkGe(expr, ctx.MkInt("0")), expr, ctx.MkMul(new ArithExpr[] { expr, ctx.MkInt("-1") }));
        }

        static void Main(string[] args)
        {
            string file = "input.txt";
            if (args.Length > 0)
            {
                file = args[0];
            }

            Console.WriteLine("Creating model from input...");
            var regex = new Regex(@"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)");
            var input = File.ReadAllLines(file);
            try
            {
                using (var ctx = new Context())
                {
                    var x    = ctx.MkIntConst("x");
                    var y    = ctx.MkIntConst("y");
                    var z    = ctx.MkIntConst("z");
                    var bots = new ArithExpr[input.Length];
                    var expr = new BoolExpr[input.Length];

                    for (int i = 0; i < expr.Length; i++)
                    {
                        var match = regex.Match(input[i]);

                        var botx   = match.Groups[1].ToString();
                        var boty   = match.Groups[2].ToString();
                        var botz   = match.Groups[3].ToString();
                        var radius = match.Groups[4].ToString();

                        var absx = GetAbsExpr(ctx, ctx.MkSub(new ArithExpr[] { x, ctx.MkInt(botx) }));
                        var absy = GetAbsExpr(ctx, ctx.MkSub(new ArithExpr[] { y, ctx.MkInt(boty) }));
                        var absz = GetAbsExpr(ctx, ctx.MkSub(new ArithExpr[] { z, ctx.MkInt(botz) }));

                        bots[i] = ctx.MkIntConst("bot_" + i);
                        expr[i] = ctx.MkEq(bots[i],
                                           ctx.MkITE(ctx.MkLe(ctx.MkAdd(new ArithExpr[] { absx, absy, absz }),
                                                              ctx.MkInt(radius)),
                                                     ctx.MkInt("1"),
                                                     ctx.MkInt("0")));
                    }

                    var opt = ctx.MkOptimize();
                    opt.Add(expr);

                    var sum = ctx.MkIntConst("sum");
                    opt.Add(new BoolExpr[] { ctx.MkEq(sum, ctx.MkAdd(bots)) });

                    var dist = ctx.MkIntConst("dist");
                    opt.Add(new BoolExpr[] { ctx.MkEq(dist, ctx.MkAdd(new ArithExpr[] { GetAbsExpr(ctx, x), GetAbsExpr(ctx, y), GetAbsExpr(ctx, z)})) });


                    Console.WriteLine("Starting Z3...");
                    opt.MkMaximize(sum);
                    var handle= opt.MkMinimize(dist);

                    Console.WriteLine("Status = {0}", opt.Check());
                    Console.WriteLine("Lower  = {0}", handle.Lower);
                    Console.WriteLine("Upper  = {0}", handle.Upper);

                    opt.Dispose();
                    ctx.Dispose();
                }
            }
            catch (Z3Exception e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine(e.StackTrace);
            }
        }
    }
}
