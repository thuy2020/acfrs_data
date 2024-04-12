con <- dbConnect(dbDriver("PostgreSQL"), 
                 host = "acfr.cluster-c30my4s8gc8w.us-west-2.rds.amazonaws.com",
                 port=5432, 
                 dbname = "cafrs",
                 user = "libertas",
                 password = "loam-guitar-bamboozle-wok"
)