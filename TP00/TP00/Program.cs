using System;
namespace TP00
{
    internal class Program
    { 
        public static void Main()
        {
            /*Console.WriteLine(IsPrime((13)));
            Console.WriteLine((IsPrime(14)));
            */
            DisplayWonderlandTime(23, 15);
            DisplayWonderlandTime(12, 45);
            DisplayWonderlandTime(14, 26);
            DisplayWonderlandTime(3, 20);

        } 
        public static void HelloWorld()
        {
        Console.WriteLine("Bonjour, le Monde");
        }
    
        public static uint Factorial(uint n)
        {
            if (n == 1)
            {
                return 1;
            }
            else
            {

                return n*Factorial(n - 1);
            }
        }

        public static bool IsPrime(uint n)
        {
            if (n == 1)
            {
                return false;
            }
            else
            {
                bool checkValue(uint x)
                {
                    if (x == 2)
                    {
                        return true;
                    }
                    else if (n % x == 0)
                    {
                        return false;
                    }
                    else
                    {
                        return checkValue(x - 1);
                    }
                }
                return checkValue(n - 1);
            }
        }

        public static uint Fibonacci (uint n)
        {  
            if (n==0  || n == 1)
            {
                return n;
            }
            else
            {
                return Fibonacci(n - 1) + Fibonacci(n - 2);
            }
        }


        public static string FizzBuzz(uint n)
        {

            if (n == 0)
            {
                return "";
            }
            else if (n % 15 == 0)
            {
                return FizzBuzz(n - 1) + " Fizz Buzz";
            } 
            else if (n % 5 == 0)
            {
                return FizzBuzz(n - 1) + " Buzz";
            } 
            else if (n % 3 == 0)
            {
                return FizzBuzz(n - 1) + " Fizz";
            }
            else
            {
                return FizzBuzz(n-1)+ " "+ n.ToString();
            }
        }

        public static void WhatTimeIsIt()
        {
            int inthour = DateTime.Now.Hour;
            int intminute = DateTime.Now.Minute;
            Console.WriteLine(inthour + ":" + intminute);
        }


        public static uint WhatQuarterIsIt(uint minutes)
        {
            if (minutes == 0 || minutes == 15 || minutes == 30 || minutes == 45)
            {
                return minutes;
            }
            else if(minutes < 15)
            {
                return 15;
            }
            else if (minutes < 30)
            {
                return 30;
            }
            else if (minutes < 45)
            {
                return 45;
            }
            else
            {
                return 0;
            }
        }
        public static uint WhatHourIsIt(uint hours)
        {
            if (hours < 12)
            {
                hours += 12;
            }
            if (hours == 12 || hours == 17 || hours == 23)
            {
                return hours;
            }
            else if (hours < 15)
            {
                return 12;
            }
            else if (hours < 19)
            {
                return 17;
            }
            else
            {
                return 23;
            }
        }


        public static void DisplayWonderlandTime(uint hours, uint minutes)
        {
            string clock1200 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10     |      2 \\
        ;;         |         ::
        || 9       O       3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1215 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;         |         ::
        || 9       O----   3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1230 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;         |         ::
        || 9       O       3 ||
        ::         |         ;;
         \\ 8      |      4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1245 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;         |         ::
        || 9   ----O       3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1700 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10     |      2 \\
        ;;         |         ::
        || 9       O       3 ||
        ::          \        ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1715 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;                   ::
        || 9       O----   3 ||
        ::          \        ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1730 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;                   ::
        || 9       O       3 ||
        ::         |\        ;;
         \\ 8      |      4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock1745 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;                   ::
        || 9   ----O       3 ||
        ::          \        ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock2300 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10     |      2 \\
        ;;        \|         ::
        || 9       O       3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock2315 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;        \          ::
        || 9       O----   3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock2330 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;        \          ::
        || 9       O       3 ||
        ::         |         ;;
         \\ 8      |      4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            string clock2345 = @"
                _____
             _.'_____`._
           .'.-'  12   `-.`.
          /,' 11        1 `.\
         // 10            2 \\
        ;;        \          ::
        || 9   ----O       3 ||
        ::                   ;;
         \\ 8             4 //
           \`. 7       5 ,'/
            '.`-.__6__.-'.'
             ((-._____.-))
             _))       ((_
             '--'SSt'--
            ";
            hours = WhatHourIsIt((hours));
            minutes = WhatQuarterIsIt(minutes);
            switch (hours)
            {
                case 12 when minutes==0 :
                    Console.WriteLine(clock1200);
                    break;
                case 12 when minutes==15 :
                    Console.WriteLine(clock1215);
                    break;
                case 12 when minutes==30 :
                    Console.WriteLine(clock1230);
                    break;
                case 12 when minutes==45 :
                    Console.WriteLine(clock1245);
                    break;
                case 17 when minutes==0 :
                    Console.WriteLine(clock1700);
                    break;
                case 17 when minutes==15 :
                    Console.WriteLine(clock1715);
                    break;
                case 17 when minutes==30 :
                    Console.WriteLine(clock1730);
                    break;
                case 17 when minutes==45 :
                    Console.WriteLine(clock1745);
                    break;
                case 23 when minutes==0 :
                    Console.WriteLine(clock2300);
                    break;
                case 23 when minutes==15 :
                    Console.WriteLine(clock2315);
                    break;
                case 23 when minutes==30 :
                    Console.WriteLine(clock2330);
                    break;
                case 23 when minutes==45 :
                    Console.WriteLine(clock2345);
                    break;
                default:
                    Console.WriteLine("Erreur ça n'existe pas au pays des rêves !");
                    break;
            }
            
        }

        public static void RabbitWhatTimeIsIt()
        {
            string rabbitTot=@"
                                 ,
                    /|      __
                   / |   ,-~ /
                  Y :|  //  /
                  | jj /( .^
                  >-"~"-v"
                 /       Y
                jo  o    |
               ( ~T~     j
                >._-' _./
               /   "~"  |
              Y     _,  |
             /| ;-"~ _  l
            / l/ ,-"~    \
            \//\/      .- \
             Y        /    Y--o
             l       I     !
             ]\      _\    /"\
            (" ~----( ~   Y.  )
        ~~~~~~~~~~~~~~~~~~~~~~~~~~
        I'm late! I'm late! For a very important date! No time to say hello,
        goodbye! I'm late! I'm late! I'm late!"
        }
        
    }
}