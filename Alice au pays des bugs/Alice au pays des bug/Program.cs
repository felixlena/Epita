using System;
using System.Linq;

namespace Alice_au_pays_des_bug
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine(Syracuse(20));

        }

        public static void PrintNatural(int n)
        {
            Console.Write(1);
            for(int i=2; i<=n; i++)
            {
                Console.Write(" "+i);
            }
        }
        public static void PrintPrimes(int n)
        {
            bool IsPrime(int i)
            {
                if (i == 1)
                {
                    return false;
                }
                if (i == 2)
                {
                    return true;
                }

                for (int x = 2; x < i; x++)
                {
                    if (i % x == 0)
                    {
                        return false;
                    }
                }
                return true;
            }

            if (n >= 2)
            {
                Console.Write(2);
            }
            for (int i = 3; i <= n; i++)
            {
                if (IsPrime(i))
                {
                    Console.Write(" " + i);
                }
            }
        }
        public static long Fibonacci(int n)
        {
            int f1 = 0, f2 = 1;
            for (int x = n; x > 2; x -= 2)
            {
                (f1, f2) = (f1 + f2, 2 * f2 + f1);
            }

            if (n % 2 == 0)
            {
                return f1;
            }
            return f2;
            
        }
        public static void PrintStrong(int n)
        {
            long Factorial(int i)
            {
                if (i == 0)
                {
                    return 1;
                }
                return i * Factorial(i - 1);
            
            }

            while (n != 0)
            {
                int z = 1;
                while (Factorial(z+1)< n) 
                {
                    z += 1;
                }
                Console.Write(z+ " ");
                n -= (int) Factorial(z);
            }
        }
        public static float Abs(float n)
        {
            if (n < 0)
            {
                return -n;
            }

            return n;
        }
        public static float Sqrt(float n)
        {
            float z = Abs(n);
            for (int i = 0; i < 10; i++)
            {
                z = 0.5f * (z + n / z);
            }
            return z;
        }
        public static long Power(long a, long b)
        {
            long x = 1;
            while (b > 0)
            {
                x *= a;
                b -= 1;
            }

            return x;
        } 
        public static void PrintTree(int n)
        {
            void Afficher(int space, int stars)
            {
                for (int x = 0; x < space; x++)
                {
                    Console.Write(" ");
                }
                for (int y = 0; y < stars; y++)
                {
                    Console.Write("*");
                }
                Console.WriteLine();
            }
            int espace = n - 1;
            int etoile = 1;
            for (int i = 1; i <= n; i+=1)
            {
                Afficher(espace,etoile);
                etoile += 2;
                espace -= 1;
            }
            /* Tronc */
            int tailletronc = 1;
            if (n > 3)
            {
                tailletronc = 2;
            }
            for (int y = 0; y < tailletronc; y++)
            {
                Afficher(n-1, 1);
            }
        }

        public static int Syracuse(int n)
        {
            int Syra(int x, int i)
            {
                if (x == 1)
                {
                    return i;
                }
                else if (x % 2 == 0)
                {
                    return Syra(x / 2, i+1);
                }

                return Syra(3 * x + 1, i+1);
            }

            return Syra(n, 0);
        }
    }
}