// Offline curl substitute: no networking APIs and reject every unknown request.
using System;
using System.IO;
using System.Text;
using System.Linq;
public class FakeCurl {
    public static int Main(string[] args) {
        Console.OutputEncoding = new UTF8Encoding(false);
        if (args.Length == 1 && args[0] == "--version") { Console.Write("fake-curl\n"); return 0; }
        string url = args.LastOrDefault() ?? "", method = "GET", body = "";
        for (int i = 0; i < args.Length - 1; i++) {
            if (args[i] == "-X") method = args[i + 1];
            if (args[i] == "--data-binary") {
                if (!args[i + 1].StartsWith("@")) return 91;
                body = File.ReadAllText(args[i + 1].Substring(1), Encoding.UTF8);
            }
        }
        File.AppendAllText(Environment.GetEnvironmentVariable("M365_FAKE_LOG"),
            method + "\t" + url + "\t" + Convert.ToBase64String(Encoding.UTF8.GetBytes(body)) + "\n");
        string json;
        if (url == "https://login.microsoftonline.com/consumers/oauth2/v2.0/token" && method == "POST"
            && body.Contains("grant_type=refresh_token") && body.Contains("refresh_token=offline-refresh"))
            json = "{\"access_token\":\"offline-access\",\"refresh_token\":\"offline-refresh\",\"expires_in\":3600,\"scope\":\"User.Read Tasks.ReadWrite\"}";
        else if (url.StartsWith("https://graph.microsoft.com/v1.0/me?") && method == "GET")
            json = "{\"id\":\"offline-user\",\"displayName\":\"年次 😀\",\"mail\":\"fake@example.invalid\"}";
        else if (url == "https://graph.microsoft.com/v1.0/me/todo/lists/offline-list/tasks" && method == "POST"
            && body.Contains("title")) json = "{\"id\":\"offline-task\",\"title\":\"年次 😀\"}";
        else if (url == "https://offline.invalid/fail") return 23;
        else { Console.Error.Write("Unexpected fake request: " + method + " " + url); return 92; }
        Console.Write("HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\n\r\n" + json);
        return 0;
    }
}
