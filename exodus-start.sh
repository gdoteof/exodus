rm /data/db/mongod.lock
mongod --rest &
yesod --dev devel 
