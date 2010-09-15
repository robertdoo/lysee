import ib;

#-------------------------------------------------------------------------------

const db:database = do
  mydb = database("ib");
  mydb.connecTo(__file__.changeExt(".fdb"), "SYSDBA", "masterkey");
  return mydb;
end
  
#-------------------------------------------------------------------------------

def openSQL:dataset |SQL:string|
  return db.openSQL(SQL);
end

#-------------------------------------------------------------------------------

def execSQL:int |SQL:string|
  return db.execSQL(SQL);
end

#-------------------------------------------------------------------------------

def COMMIT
  db.commitRetaining(); 
end


