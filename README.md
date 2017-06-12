# myphone

An R package to extract contents from unencrypted iPhone backup files (SMS messages and address book/contacts)

To create an unencrypted backup, connect your iPhone to your computer and open iTunes. Find the 
phone in the Devices list and open the main "Summary" page to see the Backup settings. Uncheck
the check box for "Encrypt iPhone backup" and then click "Back Up Now".

## Usage

Here's some sample code 

```
devtools::install_github("mrflick/myphone");

backup <- get_backup();
sms <- read_sms_data(backup);

#count sent/recieved
table(sms$type)

#distribution of sms lenth
hist(nchar(sms$text))		
```

## References
* <http://linuxsleuthing.blogspot.com/2011/02/parsing-iphone-sms-database.html>
* <http://mikebeach.org/2012/11/05/how-to-read-and-export-iphone-sms-text-messages-on-windows/>
* <http://www.wired.com/2013/11/backup-sms-iphone/>
* <http://www.iphonebackupextractor.com/blog/2012/apr/23/what-are-all-files-iphone-backup/>
* <http://forums.macrumors.com/threads/how-to-convert-addressbook-sqlitedb-to-csv-or-similar.1350341>
* <http://linuxsleuthing.blogspot.com/2012/10/addressing-ios6-address-book-and-sqlite.html>
