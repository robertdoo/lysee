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

#-------------------------------------------------------------------------------

/*
  if mydb.tables.indexOf("dwk") < 0 then
    mydb.execSQL("""CREATE TABLE dwk (
                        dw_bh   varchar(32) primary key not null,
                        dw_mc   varchar(200) not null,
                        dw_dz   varchar(200) not null,
                        dw_lxr  varchar(32) not null,
                        dw_dh   varchar(32) not null,
                        dw_lx   char(1) not null,
                        dw_zjrq char(8) not null,
                        dw_bz   varchar(200) default ''                        
                    )""");
    mydb.commitRetaining();
  end
                
  if mydb.tables.indexOf("wjk") < 0 then
    mydb.execSQL("""CREATE TABLE wjk (
                        wj_bh   varchar(128) primary key not null,
                        wj_dw   varchar(32) not null,
                        wj_mc   varchar(200) not null,
                        wj_lx   varchar(16) not null,
                        wj_nd   int not null,
                        wj_zjrq char(8) not null                        
                    )""");

    mydb.commitRetaining();
  end
*/