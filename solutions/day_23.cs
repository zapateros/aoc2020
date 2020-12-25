# I bruteforced part 2 in C#. It took about 2 hours. Also I didn't clean the code yet but here is the code that gives you part 2 and part 1 somewhere all along.
string[] lines = System.IO.File.ReadAllLines(@"input_day_24.txt");
            List<string> dirs = new List<string>() { "nw", "ne", "sw", "se", "w", "e" };
            List<int> dirx = new List<int>() { -1, 1, -1, 1, -2, 2 };
            List<int> diry = new List<int>() { -2, -2, 2, 2, 0, 0 };

            // Part 1
            List<string> tiles = new List<string>();
            foreach (string y in lines)
            {
                string line = y;
                List<string> steps = new List<string>();
                while (line.Length > 0)
                {
                    string st = line.Substring(0, 1);
                    if (st == "w" | st == "e")
                    {
                        steps.Add(st);
                        line = line.Substring(1);
                    }
                    else
                    {
                        steps.Add(line.Substring(0, 2));
                        line = line.Substring(2);
                    }
                }
                int posx = 0;
                int posy = 0;
                foreach (string x in steps)
                {
                    posx += dirx[dirs.IndexOf(x)];
                    posy += diry[dirs.IndexOf(x)];
                }
                string p = posy.ToString() + ',' + posx.ToString();
                tiles.Add(p);

            }

            List<string> black = new List<string>();
            var unique = new HashSet<string>(tiles);
            foreach (string z in unique)
            {
                int n = 0;
                foreach (string k in tiles)
                {
                    if (k == z)
                    {
                        n++;
                    }
                }
                if (n % 2 == 1)
                {
                    black.Add(z);
                }
            }


            List<string> nbs_1 = new List<string>();
            List<string> nbs_2 = new List<string>();

            string[] subs = black[0].Split(',');
            int yco = int.Parse(subs[0], System.Globalization.NumberStyles.AllowLeadingSign);
            int xco = int.Parse(subs[1], System.Globalization.NumberStyles.AllowLeadingSign);

            int[] sumy = { -2, -2, 0, 2, 2, 0 };
            int[] sumx = { -1, 1, 2, 1, -1, -2 };

            Console.WriteLine(black[0]);
            for (int a = 0; a < sumy.Length; a++)
            {
                int yco_add = yco + sumy[a];
                int xco_add = xco + sumx[a];
                string pp = yco_add.ToString() + ',' + xco_add.ToString();

                nbs_1.Add(black[0]);
                nbs_2.Add(pp);
               
            }
            Console.WriteLine(pp);

    */


            //int[] cups = new int[] { 9, 1, 6, 4, 3, 8, 2, 7, 5 };
            var cups = new List<int> { 9, 1, 6, 4, 3, 8, 2, 7, 5 };
            //var cups = new List<int> { 3,2,8,1,4,7,5,9,6 };
            
            for(int i=10; i <=1000000; i++)
            {
                cups.Add(i);
            }
            //Console.WriteLine(cups.Count);
            int it = 0;
            while(it < 10000000)
            {
                int rel = cups[0];
                List<int> cups_pick = cups.GetRange(1, 3);
                cups.RemoveRange(1, 3);

                int dest = rel;
                while (true)
                {
                    dest = dest - 1;
                    if(dest == 0)
                    {
                        dest = 1000000;
                    }
                    if (cups_pick.IndexOf(dest) == -1)
                    {
                        break;
                    }
                }
                
                cups.RemoveAt(0);
                cups.Add(rel);
                int ind = cups.IndexOf(dest) + 1;
                cups.InsertRange(ind, cups_pick);
                //string test = string.Concat(cups);

                //Console.WriteLine(test);
                it++;
            }

            int index_one = cups.IndexOf(1);
            var cups_after = cups.GetRange(index_one, 3);
            foreach(int u in cups_after)
            {
                Console.WriteLine(u);
            }
