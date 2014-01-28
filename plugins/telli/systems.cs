using System;
using System.Reflection;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace __telli__
{
    public class SimpleMemberInfo
    {
        public string ClassName
        {
            get;set;
        }
        public Member Member
        {
            get;set;
        }
        public string FullClassName
        {
            get;set;
        }
    }

    public class Parameters
    {
        
    }

    public class Member
    {
        public string Return
        {
            get;set;
        }

        public string Name
        {
            get;set;
        }

        public bool Property
        {
            get;set;
        }

        public bool Field
        {
            get;set;
        }
    }

    public class Class
    {
        public Class()
        {
            this.Members = new List<Member>();
            this.Interfaces = new List<string>();
        }

        public string Namespace
        {
            get;set;
        }

        public string Descendant
        {
            get;set;
        }
        
        public string Name
        {
            get;set;
        }

        public string Base
        {
            get;set;
        }

        public string Template
        {
            get;set;
        }

        public List<Member> Members
        {
            get;set;
        }

        public List<string> Interfaces
        {
            get;set;
        }
    }

    public class Systems
    {
        List<Class> classes = new List<Class>();
        Dictionary<string,List<string>> namespaces = new Dictionary<string,List<string>>();

        public Class get_class(string name)
        {
            Class cls = get_class_no_error(name);

            if (cls == null)
            {
                cls = get_class_no_error(name + "<T>");
            }

            if (cls == null)
            {
                // Console.WriteLine("No class named '" + name + "'");
                throw new Exception("Class " + name + " not found");
            }
            return cls;
        }

        public Class get_class_no_error(string name)
        {
            string template = string.Empty;
            name = check_templates(name, ref template, true);
            name = check_name(name);
            
            // Class cls = classes
            Class cls = 
            // var gas =
                classes
                .Where(c => c.Name.Equals(name, StringComparison.CurrentCultureIgnoreCase))
                .FirstOrDefault();
                // .SingleOrDefault();
                // .ToList();
            // Console.WriteLine("gas: " + gas[0].Namespace + "." + gas[0].Name);
            // Console.WriteLine("gas: " + gas[1].Namespace + "." + gas[1].Name);

            if (cls != null)
            {
                cls.Template = template;
            }

            return cls;
        }

        public string retrieve_members(string name)
        {            
            Class cls = get_class_no_error(name);

            if (cls != null)
            {
                return string.Join("", cls.Members.Select(m => m.Name + "\n")
                                   .Distinct()
                                   .ToArray());
            }
            else
            {
                // Console.WriteLine("Desc: " + classes
                //                   .Where(cl => cl.Descendant == name).Count());
                List<string> ret = new List<string>
                    (classes
                     .Where(cl => cl.Descendant == name)
                     .Select(cl => cl.Name + "\n"));

                // Console.WriteLine("Ret: " + ret.Count);

                if (namespaces.ContainsKey(name))
                {
                    ret.InsertRange(ret.Count, namespaces[name].Select(m => m + "\n"));
                    // Console.WriteLine("Ret: " + ret.Count);
                }
                if (ret.Count == 0)
                {
                    throw new Exception(name + " not recognized");
                }
                return string.Join("", ret // namespaces[name].Select(m => m + "\n")
                                   .ToArray());
            }
        }

        public string get_full_name(string name)
        {
            Class cls = get_class(name);
            string ret = cls.Namespace + "." + cls.Name;
            // Console.WriteLine(ret);
            return ret;
        }

        public string get_return_class(string name, string member)
        {
            Class cls = get_class_no_error(name);

            if (cls == null)
            {
                // Console.WriteLine("No class named '" + name + "'");
                return member + "\n";
            }

            string mem_template = string.Empty;
            member = check_templates(member, ref mem_template, false);

            Member mem = cls.Members
                .Where(m => m.Name.Equals(member, StringComparison.CurrentCultureIgnoreCase))
                .FirstOrDefault();

            if (mem == null)
            {
                throw new Exception(member + " not part of " + name);
            }

            string t_out = string.Empty;
            string ret = check_return(mem.Return, ref t_out);
            
            if (cls.Template.Length > 0 && ret == "T")
            {
                return cls.Template + "\n";
            }

            if (t_out.Length > 0)
            {
                if (t_out == "TOutput")
                {
                    return ret + "<" + mem_template + ">" + "\n";
                }
                else 
                {
                    return ret + "<" + cls.Template + ">" + "\n";
                }
            }
            else
            {
                string pattern = @"(?<pre>.*)\bT\b(?<post>.*)";
                string output = string.Empty;
                if (check_return_template(cls.Template, ret, pattern, ref output))
                {
                    return output + "\n";
                }

                pattern = @"(?<pre>.*)\bTOutput\b(?<post>.*)";

                if (check_return_template(mem_template, ret, pattern, ref output))
                {
                    return output + "\n";
                }
            }

            return ret + "\n";
        }

        bool check_return_template(string temp, string ret, string pattern,
                                   ref string output)
        {
            string pre = string.Empty, post = string.Empty;
            if (pre_post_regex(temp, ret, pattern, ref pre, ref post))
            {
                output = pre + temp + post;
                return true;
            }
            return false;
        }

        bool pre_post_regex(string temp, string ret, string pattern,
                            ref string pre, ref string post)
        {
            Regex rx = new Regex(pattern);
            if (rx.IsMatch(ret))
            {
                Match m = rx.Match(ret);
                pre = m.Result("${pre}");
                post = m.Result("${post}");
                return true;
            }
            return false;
        }

        string check_return(string ret, ref string t_out)
        {
            string pattern = @"(?<type>[^\[]+)`1.*\[(?<template>[^\]]+)";
            Regex rx = new Regex(pattern);

            if (rx.IsMatch(ret))
            {
                Match m = rx.Match(ret);
                ret = m.Result("${type}");
                t_out = m.Result("${template}");
            }
            return ret;
        }

        string check_templates(string name, ref string template, bool drull)
        {
            string pattern = "(?<type>[^<]+)<(?<template>[^>]+)";
            Regex rx = new Regex(pattern);

            if (rx.IsMatch(name))
            {
                Match m = rx.Match(name);
                name = m.Result("${type}") + (drull ? "`1" : "");
                template = m.Result("${template}");
            }
            return name;
        }

        // public string get_class(string name, string member, ref bool property)
        public SimpleMemberInfo get_smi(string name, string member)
        {
            Class cls = get_class(name);

            SimpleMemberInfo smi = new SimpleMemberInfo();
            smi.FullClassName = cls.Namespace + "." + cls.Name;
            smi.ClassName = name;

            string mem_template = string.Empty;
            member = check_templates(member, ref mem_template, false);

            Member mem = get_member(name, member);
            if (mem == null)
            {
                foreach (string iface in cls.Interfaces)
                {
                    mem = get_member(iface, member);
                    if (mem != null)
                    {
                        smi.FullClassName = get_full_name(iface);
                        smi.ClassName = iface;
                        break;
                    }
                }
            }
            smi.Member = mem;
            return smi;
        }

        Member get_member(string name, string member)
        {
            Class cls = get_class_no_error(name);

            if (cls == null)
            {
                // Console.WriteLine("No class named '" + name + "'");
                return null;
            }

            Member mem = cls.Members
                .Where(m => m.Name.Equals(member, StringComparison.CurrentCultureIgnoreCase))
                .FirstOrDefault();

            return mem;
        }
        
        public void load_assembly(string file)
        {
            // Read file and change xml
            Assembly assembly = Assembly.LoadFrom(file);
            // Type[] types = assembly.GetTypes();
            Type[] types = assembly.GetExportedTypes();

            foreach (Type t in types)
            {
                // if (true //false
                //     || t.Name.Equals("object", StringComparison.CurrentCultureIgnoreCase)
                //     || t.Name.Equals("Array", StringComparison.CurrentCultureIgnoreCase)
                //     || t.Name.Equals("string", StringComparison.CurrentCultureIgnoreCase)
                //     || t.Name.Equals("IList", StringComparison.CurrentCultureIgnoreCase)
                //     )
                {
                    Class cls = new Class();
                    cls.Namespace = t.Namespace;
                    cls.Name = t.Name;
                    cls.Descendant = process_namespace(t.Namespace, t.Name);
                    // Console.WriteLine(cls.Descendant + "-" + cls.Name);
                    cls.Base = t.BaseType != null ? t.BaseType.ToString() : "";
                    classes.Add(cls);

                    // Console.WriteLine(t.Namespace + ":" + t.Name + " - " + cls.Base);
                    
                    get_members(t, cls);

                    get_interfaces(t, cls);

                    // Console.WriteLine("Done");

                }
            }
        }

        string process_namespace(string ns, string name)
        {
            string[] sns = ns.Split('.');

            for (int i=1; i < sns.Length; i++)
            {
                if (!namespaces.ContainsKey(sns[i-1]))
                {
                    // Console.WriteLine("Adding key: " + sns[i-1]);
                    namespaces[sns[i-1]] = new List<string>();                    
                }
                if (!namespaces[sns[i-1]].Contains(sns[i]))
                {
                    // Console.WriteLine("Adding " + sns[i] + " to " + sns[i-1]);
                    namespaces[sns[i-1]].Add(sns[i]);
                }
            }
            string last = sns[sns.Length - 1];
            return last;
            // if (!namespaces.ContainsKey(last))
            // {
            //     // Console.WriteLine("Adding key: " + last);
            //     namespaces[last] = new List<string>();
            // }
            // if (!namespaces[last].Contains(name))
            // {
            //     // Console.WriteLine("Adding " + name + " to " + last);
            //     namespaces[last].Add(name);                
            // }
        }

        void get_members(Type t, Class cls)
        {
            MemberInfo[] members = t.GetMembers();
            foreach (MemberInfo member in members)
            {
                if (member.MemberType == MemberTypes.Method)
                {
                    if (member.Name.IndexOf("get_") == 0 ||
                        member.Name.IndexOf("set_") == 0)
                    {
                        continue;
                    }

                    Member mem = new Member();
                    mem.Return = get_return_type(member);
                    mem.Name = member.Name;
                    cls.Members.Add(mem);

                    // if (false && cls.Name == "String")
                    // {
                    //     Console.Write("    " + member.DeclaringType.FullName + " - " + 
                    //                   member.Name);
                    //     Console.WriteLine("    " + mem.Return);
                    // }

                    get_parameters(member, cls);
                }
                else if (member.MemberType == MemberTypes.Property)
                {
                    Member mem = new Member();
                    mem.Property = true;
                    mem.Return = get_return_type(member);
                    mem.Name = member.Name;
                    // Console.WriteLine("Prop: " + mem.Return + " " + mem.Name);
                    cls.Members.Add(mem);
                }
                else if (member.MemberType == MemberTypes.Field)
                {
                    Member mem = new Member();
                    mem.Field = true;
                    mem.Return = get_return_type(member);
                    mem.Name = member.Name;
                    // Console.WriteLine("Field: "+ mem.Return + "  " + mem.Name);
                    cls.Members.Add(mem);
                }
                else
                {
                    // Console.WriteLine("    " +
                    //                   member.Name + " - " + 
                    //                   member.MemberType);
                }
            }
        }

        void get_interfaces(Type t, Class cls)
        {
            Type[] interfaces = t.GetInterfaces();
            foreach (Type iface in interfaces)
            {
                // Console.WriteLine(iface.Name);
                cls.Interfaces.Add(iface.Name);
            }
        }

        string get_return_type(MemberInfo member)
        {
            string return_type = string.Empty;
            if (member is MethodInfo)
            {
                return_type = (member as MethodInfo).ReturnType.ToString();
            }
            else if (member is PropertyInfo)
            {
                return_type = (member as PropertyInfo).PropertyType.ToString();
            }
            else if (member is FieldInfo)
            {
                return_type = (member as FieldInfo).FieldType.ToString();
            }
            return_type = return_type.Remove(0, return_type.LastIndexOf('.') + 1);
            return_type = correct_type(return_type);
            return return_type;
        }

        void get_parameters(MemberInfo mi, Class cls)
        {
            // ParameterInfo[] paramss = (member as MethodInfo).GetParameters();
            // foreach (ParameterInfo param in paramss)
            // {
            //     string type = param.ParameterType.ToString();
            //     type = type.Remove(0, type.LastIndexOf('.') + 1);
            //     type = correct_type(type);
            //     // type = type.Split('.').ToList().Last();
            //     // Console.WriteLine("        " + 
            //     //                   type + 
            //     //                   " " + param.Name);
            // }
        }

        string check_name(string type)
        {
            switch (type)
            {
                case "int":
                    return "Int32";
                case "uint":
                    return "UInt32";
                case "short":
                    return "Int16";
                case "ushort":
                    return "UInt16";
                case "bool":
                    return "Boolean";
            }
            return type;
        }

        public static string correct_type(string type)
        {
            string ret = string.Empty;
            
            string pattern = "(?<type>[a-zA-Z_][a-zA-Z0-9_]*)" 
                + "((?<extra>\\[\\])|(?<amp>&))?";

            Regex rx = new Regex(pattern);
            if (rx.IsMatch(type))
            {
                Match m = rx.Match(type);
                ret = m.Result("${type}");
                ret = normalize_type(ret);
                ret += m.Result("${extra}");
                if (m.Result("${amp}").Length > 0)
                {
                    ret = "ref " + ret;
                }
            }
            return ret;
        }

        public static string normalize_type(string type)
        {
            switch (type)
            {
                case "String":
                    return "string";
                case "Object":
                    return "object";
                case "Int32":
                    return "int";
                case "UInt32":
                    return "uint";
                case "Int16":
                    return "short";
                case "UInt16":
                    return "ushort";
                case "Char":
                    return "char";
                case "Double":
                    return "double";
                case "Byte":
                    return "Byte";
                case "Boolean":
                    return "bool";
                default:
                    return type;
            }
        }
        
    }
}