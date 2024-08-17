-module(practical_list_comprehensions).
-export([run_examples/0]).

run_examples() ->
    io:format("1. Processamento de dados de um arquivo CSV~n"),
    process_csv_file(),
    
    io:format("~n2. Análise de logs de servidor web~n"),
    analyze_web_logs(),
    
    io:format("~n3. Processamento de dados de um crawler web (simulado)~n"),
    process_web_crawler_data(),
    
    io:format("~n4. Transformação de dados JSON~n"),
    transform_json_data(),
    
    io:format("~n5. Filtragem e agregação de dados de sensores~n"),
    process_sensor_data().

% Função auxiliar para ler arquivo
%read_file(Filename) ->
%    {ok, Binary} = file:read_file(Filename),
%    string:tokens(binary_to_list(Binary), "\r\n").

% 1. Processamento de dados de um arquivo CSV
process_csv_file() ->
    % Simula leitura de um arquivo CSV
    CSVData = ["Name,Age,City", 
               "Alice,28,New York", 
               "Bob,35,Los Angeles", 
               "Charlie,42,Chicago"],
    
    % Processa o CSV usando lista compreensiva
    ParsedData = [parse_csv_line(Line) || Line <- CSVData, Line /= "Name,Age,City"],
    
    % Filtra pessoas com mais de 30 anos
    Over30 = [{Name, Age} || {Name, Age, _} <- ParsedData, Age > 30],
    
    io:format("Pessoas com mais de 30 anos: ~p~n", [Over30]).

parse_csv_line(Line) ->
    [Name, AgeStr, City] = string:tokens(Line, ","),
    {Name, list_to_integer(AgeStr), City}.

% 2. Análise de logs de servidor web
analyze_web_logs() ->
    % Simula leitura de um arquivo de log
    LogLines = [
        "192.168.1.1 - - [10/Oct/2023:13:55:36 +0000] \"GET /index.html HTTP/1.1\" 200 2326",
        "192.168.1.2 - - [10/Oct/2023:13:56:03 +0000] \"GET /about.html HTTP/1.1\" 200 1432",
        "192.168.1.1 - - [10/Oct/2023:13:56:12 +0000] \"GET /images/logo.png HTTP/1.1\" 200 6573",
        "192.168.1.3 - - [10/Oct/2023:13:57:01 +0000] \"GET /index.html HTTP/1.1\" 200 2326",
        "192.168.1.4 - - [10/Oct/2023:13:57:23 +0000] \"POST /login HTTP/1.1\" 302 0"
    ],
    
    % Extrai IPs e páginas acessadas
    Requests = [{IP, Page} || LogLine <- LogLines,
                              [IP, _, _, _, Request, _] <- [string:tokens(LogLine, " ")],
                              [_, Page | _] <- [string:tokens(Request, " ")]],
    
    % Conta acessos por página
    PageCounts = [{Page, length([1 || {_, P} <- Requests, P == Page])} || 
                  Page <- lists:usort([P || {_, P} <- Requests])],
    
    io:format("Contagem de acessos por página: ~p~n", [PageCounts]).

% 3. Processamento de dados de um crawler web (simulado)
process_web_crawler_data() ->
    % Simula dados coletados por um crawler
    CrawledData = [
        {url, "http://example.com", content, "<html><body><h1>Example</h1><p>Content</p></body></html>"},
        {url, "http://example.com/about", content, "<html><body><h1>About</h1><p>About us</p></body></html>"},
        {url, "http://example.com/contact", content, "<html><body><h1>Contact</h1><p>Email us</p></body></html>"}
    ],
    
    % Extrai títulos das páginas
    Titles = [{Url, extract_title(Content)} || {url, Url, content, Content} <- CrawledData],
    
    io:format("Títulos extraídos: ~p~n", [Titles]).

extract_title(Html) ->
    case re:run(Html, "<h1>(.*?)</h1>", [{capture, [1], list}]) of
        {match, [Title]} -> Title;
        _ -> "No title found"
    end.

% 4. Transformação de dados JSON
transform_json_data() ->
    % Simula dados JSON (representados como termos Erlang)
    JsonData = [
        {struct, [{"name", "Alice"}, {"age", 28}, {"skills", ["Erlang", "Python"]}]},
        {struct, [{"name", "Bob"}, {"age", 35}, {"skills", ["Java", "C++"]}]},
        {struct, [{"name", "Charlie"}, {"age", 42}, {"skills", ["Erlang", "Elixir"]}]}
    ],
    
    % Transforma os dados
    TransformedData = [{proplists:get_value("name", Props), 
                        proplists:get_value("skills", Props)} ||
                       {struct, Props} <- JsonData,
                       proplists:get_value("age", Props) > 30],
    
    io:format("Dados JSON transformados: ~p~n", [TransformedData]).

% 5. Filtragem e agregação de dados de sensores
process_sensor_data() ->
    % Simula leituras de sensores
    SensorData = [
        {sensor1, [{timestamp, 1633872000, temperature, 22.5},
                   {timestamp, 1633875600, temperature, 23.1},
                   {timestamp, 1633879200, temperature, 22.8}]},
        {sensor2, [{timestamp, 1633872000, temperature, 21.8},
                   {timestamp, 1633875600, temperature, 22.3},
                   {timestamp, 1633879200, temperature, 22.0}]}
    ],
    
    % Calcula a média de temperatura para cada sensor
    AverageTemps = [{Sensor, lists:sum([Temp || {timestamp, _, temperature, Temp} <- Readings]) / length(Readings)} ||
                    {Sensor, Readings} <- SensorData],
    
    io:format("Temperaturas médias por sensor: ~p~n", [AverageTemps]).