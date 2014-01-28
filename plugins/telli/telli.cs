/* -*- compile-command: "gmcs -pkg:dotnet35 telli.cs systems.cs" -*- */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;
using System.IO;
using System.Text.RegularExpressions;
using System.Threading;

namespace __telli__
{
    class __program__
    {
        static XDocument get_xdoc(string xml)
        {
            string pre = "C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\";
            // string pre = "extra/";
            string file = pre + xml;
            string text = File.ReadAllText(file);

            text = replace_pattern(text, "<see cref=\".*\\.([^\\s]+)\" />");
            text = replace_pattern(text, "<paramref name=\".*\\.([^\\s]+)\" />");

            XDocument doc = XDocument.Parse(text);
            return doc;
        }

        static void Main(string[] args)
        {
			try
			{
                Systems systems = new Systems();
                string pre = "C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\";
                // string pre = "/usr/lib/mono/2.0/";
                systems.load_assembly(pre + "mscorlib.dll");
                systems.load_assembly(pre + "System.dll");
                systems.load_assembly(pre + "System.ServiceModel.dll");
                systems.load_assembly(pre + "System.IdentityModel.dll");
                systems.load_assembly(pre + "System.Data.dll");
                systems.load_assembly(pre + "System.configuration.dll");
                
                List<XDocument> docs = new List<XDocument>();
                docs.Add(get_xdoc("mscorlib.xml"));
                docs.Add(get_xdoc("System.xml"));
                docs.Add(get_xdoc("System.ServiceModel.xml"));
                docs.Add(get_xdoc("System.IdentityModel.xml"));
                docs.Add(get_xdoc("System.Data.xml"));
                docs.Add(get_xdoc("System.Configuration.xml"));
                // XDocument doc = XDocument.Load(file);

				while (true)
				{
                    get_and_parse_input(systems, docs);
                }
			}
			catch (Exception e)
			{
                string ret = "<error>\n";
                ret += e.Message + "\n";
                // ret += e.StackTrace + "\n";
                ret += "<end>\n";
				Console.Write(ret);
			}
        }

        static void get_and_parse_input(Systems systems, List<XDocument> docs)
        {
            string ret = "";

            try
            {
                string line = Console.ReadLine();
                string[] query = line.Split('-');

                if (query.Length < 2)
                {
                    throw new Exception("Bad query");
                }
                else if (query[0] == "qi")
                {
                    ret += query_info(systems, docs, query);
                }
                else if (query[0] == "si")
                {
                    ret += query_simple_info(systems, docs, query);
                }
                else if (query[0] == "ri")
                {
                    ret += return_info(systems, query);
                }
                else if (query[0] == "mi")
                {
                    ret += member_info(systems, query);
                }
                else if (query[0] == "pi")
                {
                    ret += param_info(systems, docs, query);
                }
                ret += "<end>\n";
                Console.Write(ret);
            }
            catch (Exception e)
            {
                ret = "<error>\n";
                ret += e.Message + "\n";
                // ret += e.StackTrace + "\n";
                ret += "<end>\n";
                Console.Write(ret);
            }
            
        }

        static string query_info(Systems systems, List<XDocument> docs, string[] query)
        {
            string ret = "<qi>\n";
            string qry = string.Empty;
            string[] spliff = query[1].Split('.');

            if (spliff.Count() == 1)
            {
                string tmp = systems.get_full_name(query[1]);
                if (tmp.Length < 1)
                {
                    throw new Exception("No information about " + query[1]);
                }
                else
                {
                    qry = "T:" + tmp;
                }
            }
            else if (spliff.Count() == 2)
            {
                SimpleMemberInfo smi = systems.get_smi(spliff[0], spliff[1]);
                // Console.WriteLine("org class: " + cls);
                // string tmp = systems.get_full_name(cls);
                // Console.WriteLine("full name: " + tmp);

                if (smi.FullClassName.Length < 1 || smi.Member == null)
                {
                    throw new Exception("No information about " + query[1]);
                }
                else
                {
                    string type = smi.Member.Property ? "P:" : smi.Member.Field ? "F:" : "M:";
                    qry = type + smi.FullClassName + "." + spliff[1];
                }
            }

            if (qry.Length > 0)
            {
                ret += get_quick_info(docs, qry);
            }
            return ret;
        }

        static string query_simple_info(Systems systems, List<XDocument> docs, string[] query)
        {
            string ret = "<si>\n";
            string[] spliff = query[1].Split('.');

            if (spliff.Count() == 2)
            {
                SimpleMemberInfo smi = systems.get_smi(spliff[0], spliff[1]);
                // Console.WriteLine("org class: " + cls);
                // string tmp = systems.get_full_name(cls);
                // Console.WriteLine("full name: " + tmp);

                if (smi.FullClassName.Length < 1 || smi.Member == null)
                {
                    throw new Exception("No information about " + query[1]);
                }
                else
                {
                    ret += smi.Member.Return + " " + smi.Member.Name;
                    if (!(smi.Member.Property || smi.Member.Field))
                    {
                        ret += "(...)";
                    }
                    ret += "\n";
                    string type = smi.Member.Property ? "P:" : smi.Member.Field ? "F:" : "M:";
                    string qry = type + smi.FullClassName + "." + spliff[1];
                    ret += get_quick_simple_info(docs, qry);
                }
            }
            else
            {
                throw new Exception("Unable to get information about " + query[1]);
            }
            return ret;
        }

        static string return_info(Systems systems, string[] query)
        {
            string ret = "<ri>\n";
            string[] spliff = query[1].Split('.');
                        
            if (spliff.Count() == 2)
            {
                // string cls = systems.get_class(spliff[0], spliff[1]);
                // Console.WriteLine("org class: " + cls);
                // string tmp = systems.get_full_name(cls);
                // Console.WriteLine("full name: " + tmp);
                // if (tmp.Length < 1)
                // {
                //     Console.WriteLine("No information about " + query[1]);
                //     continue;
                // }
                ret += systems.get_return_class(spliff[0], spliff[1]);
            }
            else
            {
                throw new Exception("Wrong format: " + query[1]);
            }
            return ret;   
        }

        static string member_info(Systems systems, string[] query)
        {
            string ret = "<mi>\n";
            ret += systems.retrieve_members(query[1]);
            return ret;
        }

        static string param_info(Systems systems, List<XDocument> docs, string[] query)
        {
            string ret = "<pi>\n";
            string[] spliff = query[1].Split('.');

            if (spliff.Count() == 1)
            {
                string tmp = systems.get_full_name(query[1]);
                if (tmp.Length < 1)
                {
                    throw new Exception("No information about " + query[1]);
                }
                else
                {
                    string qry = "M:" + tmp + ".#ctor";
                    ret += query[1] + "(...)\n";
                    ret += get_param_info(docs, qry);
                }
            }
            else if (spliff.Count() == 2)
            {
                SimpleMemberInfo smi = systems.get_smi(spliff[0], spliff[1]);

                if (smi.FullClassName.Length < 1 || smi.Member == null)
                {
                    throw new Exception("No information about " + query[1]);
                }
                else if (smi.Member.Property)
                {
                    ret += smi.Member.Return + " " + smi.Member.Name + "\n";
                }
                else if (smi.Member.Field)
                {
                    ret += smi.Member.Return + " " + smi.Member.Name + "\n";
                }
                else
                {
                    ret += get_param_info(docs, smi);
                }
            }

            return ret;
        }

        static string replace_pattern(string text, string pattern)
        {
            string replacement = "$1";
            Regex rgx = new Regex(pattern);
            string result = rgx.Replace(text, replacement);
            return result;
        }

        static string get_quick_info(List<XDocument> docs, string query)
        {
            string ret = string.Empty;

            var members = 
                from doc in docs
                from m in doc.Descendants("member")
                where m.Attribute("name").Value.Contains(query)
                select m;

            string pre = "    ";
            foreach (var mm in members)
            {
                string name = mm.Attribute("name").Value;

                // string query = "T:System.String";
                // string name = "T:System.StringCo";
                // Regex rx = new Regex(query + @"[^a-zA-Z0-9]+");
                Regex rx = new Regex(query + @"\b");
                // rx.IsMatch(name).Dump();

                if (rx.IsMatch(name))
                {
                    ret += name + "\n";
                    ret += pre + Wrap(mm.Element("summary").Value.Trim(), 70, pre);
                    ret += "\n";
                    if (mm.Element("returns") != null)
                    {
                        ret += "\n";
                        ret += pre + Wrap("Returns: " + mm.Element("returns").Value, 70, pre); 
                        ret += "\n";
                    }

                    var parameters = mm.Descendants("param");
                    foreach (var param in parameters)
                    {
                        ret += pre + Wrap(param.Attribute("name").Value + 
                                          ": " + param.Value.Trim(), 70, pre);
                        ret += "\n";
                    }
                    ret += "\n";
                }
            }
            return ret;            
        }

        static string get_quick_simple_info(List<XDocument> docs, string query)
        {
            string ret = string.Empty;

            var mm = 
                (from doc in docs
                 from m in doc.Descendants("member")
                 where m.Attribute("name").Value.Contains(query)
                 select m).FirstOrDefault();

            if (mm == null)
            {
                return "No information for query\n";
            }

            string pre = " ";

            string name = mm.Attribute("name").Value;

            Regex rx = new Regex(query + @"\b");

            if (rx.IsMatch(name))
            {
                ret += pre + Wrap(mm.Element("summary").Value.Trim(), 60, pre);
                ret += '\n';
            }
            return ret;            
        }

        static string get_param_info(List<XDocument> docs, SimpleMemberInfo smi)
        {
            string ret = string.Empty;
            string query = "M:" + smi.FullClassName + "." + smi.Member.Name;

            ret = smi.Member.Return + " " + smi.Member.Name + "(...)\n";

            ret += get_param_info(docs, query);
            return ret;
        }

        static string get_param_info(List<XDocument> docs, string query)
        {
            string ret = string.Empty;
            var members = 
                from doc in docs
                from m in doc.Descendants("member")
                where m.Attribute("name").Value.Contains(query)
                select m;

            // List<string> param_help = new List<string>();
            Dictionary<string,string> par = new Dictionary<string, string>();

            string pre = "    ";
            foreach (var mm in members)
            {
                string name = mm.Attribute("name").Value;

                Regex rx = new Regex(query + @"\((?<param>.*)\)");
                if (rx.IsMatch(name))
                {
                    Match m = rx.Match(name);
                    string param = m.Result("${param}");
                    string[] paramstexts = param.Split(',');
                    
                    var parameters = mm.Descendants("param").ToList();

                    ret += pre + "(";
                    for (int i = 0; i < paramstexts.Length; i++)
                    {
                        string pname = parameters[i].Attribute("name").Value;
                        if (i > 0)
                        {
                            ret += ", ";
                        }
                        ret += Systems.correct_type(paramstexts[i]
                                                    .Remove(0, paramstexts[i]
                                                            .LastIndexOf('.') + 1)) + 
                            " " + pname;

                        par[pname] = pre + Wrap(pname + ": " + 
                                                  parameters[i].Value.Trim(), 60, pre + pre);
                    }
                    ret += ")\n";
                }
            }

            ret += "\n";
            foreach (string p in par.Keys)
            {
                ret += par[p] + "\n";
            }

            return ret;            
        }

        public static string Wrap(string str, int maxLength, string prefix)
        {
            if (string.IsNullOrEmpty(str)) 
            {
                return "";
            }

            if (maxLength <= 0) 
            {
                return prefix + str;
            }

            var lines = new List<string>();

            // breaking the string into lines makes it easier to process.
            foreach (string line in str.Split("\n".ToCharArray()))
            {
                var remainingLine = line.Trim();
                do
                {
                    var newLine = GetLine(remainingLine, maxLength - prefix.Length);
                    lines.Add(newLine);
                    remainingLine = remainingLine.Substring(newLine.Length).Trim();
                    // Keep iterating as int as we've got words remaining 
                    // in the line.
                } while (remainingLine.Length > 0);
            }

            return string.Join(Environment.NewLine + prefix, lines.ToArray());
        }

        private static string GetLine(string str, int maxLength)
        {
            // The string is less than the max length so just return it.
            if (str.Length <= maxLength) 
            {
                return str;
            }

            // Search backwords in the string for a whitespace char
            // starting with the char one after the maximum length
            // (if the next char is a whitespace, the last word fits).
            for (int i = maxLength; i >= 0; i--)
            {
                if (char.IsWhiteSpace(str[i]))
                {
                    return str.Substring(0, i).TrimEnd();
                }
            }

            // No whitespace chars, just break the word at the maxlength.
            return str.Substring(0, maxLength);
        } 
    }
}