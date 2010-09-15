/*
 * [2008.2.17]
 *
 * translate database tables into SQL script
 *
 * supports: Microsoft SQL server
 *
 * last modified: 2010.7.14
 *
 */

import adodbv;
 
def get_table_fields |db, table_name|
  return db.openSQL(@"SELECT * FROM INFORMATION_SCHEMA.COLUMNS
                       WHERE TABLE_NAME='%(table_name)'
                       ORDER BY ORDINAL_POSITION");
end

def mssql_wrap_table |db, table_name, script|
  script.add(@"CREATE TABLE %(table_name) (");
  
  key = db.openSQL(@"SELECT COLUMN_NAME 
                      FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE 
                      WHERE TABLE_NAME='%(table_name)' 
                      ORDER BY ORDINAL_POSITION").string(0).lower();
      
  counter = 0;
  
  for ds in get_table_fields(db, table_name) do
    name = ds.COLUMN_NAME.lower();
    defs = "    " + name + " " + ds.DATA_TYPE.upper();
                            
    max_len = ds.CHARACTER_MAXIMUM_LENGTH;
    defs += "(" + max_len + ")" if max_len > 0;
        
    defv = ds.COLUMN_DEFAULT;
    defs += " DEFAULT " + defv.copy(1, length(defv) - 2) if defv and defv[0] == '(';
    defs += " PRIMARY KEY" if name == key;
    defs += " NOT NULL" if ds.IS_NULLABLE.lower().trim() == "no";
        
    if counter > 0 then
      index = length(script) - 1;
      script[index] += ",";
    end
    ++ counter;
        
    script.add(defs);
  end

  script.add(");");
  script.add("");
end

def mssql_wrap_db |db|
  SQL = strlist();
  for table in db.tables do
    mssql_wrap_table(db, table, SQL);
  end
  return SQL;
end

= "Welcome using Microsoft SQL Server database wraper\n\n";

= "Please enter host address(127.0.0.1): ";
host = sys::readln().trim() or "127.0.0.1";

= "Please enter database name(master): ";
target = sys::readln().trim() or "master";

= "Please enter login name(sa): ";
user = sys::readln().trim() or "sa";

= "Please enter login password(NONE): ";
password = sys::readln().trim();

try
  = "\nConnecting to database ... ";
  db = database("mssql");
  db.connecTo(target, user, password, host);
  = "done\n";
  
  = @"Wraping database \"%(target)\"... ";
  mssql_wrap_db(db).saveToFile(target + ".sql");
  = "done\n";
except
  = __error__.text, eol;
end
