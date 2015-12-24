# myphone

An R package to extract contents from unencrypted iPhone backup files (SMS messages and address book/contacts)

## Usage

Here's some sample code 

```
devtools::install_github("mrflick/myphone");

backups <- list_backups();
if ( length(backups)>0 ) {
	bkfile <- backups[1];
	sms <- read_sms_data(bkfile);

	#count sent/recieved
	table(sms$Type)

	#distribution of sms lenth
	hist(nchar(sms$Text))
		
} else {
	print("No backups found!");
}
```

## References
* <http://linuxsleuthing.blogspot.com/2011/02/parsing-iphone-sms-database.html>
* <http://mikebeach.org/2012/11/05/how-to-read-and-export-iphone-sms-text-messages-on-windows/>
* <http://www.wired.com/2013/11/backup-sms-iphone/>
* <http://www.iphonebackupextractor.com/blog/2012/apr/23/what-are-all-files-iphone-backup/>
* <http://forums.macrumors.com/threads/how-to-convert-addressbook-sqlitedb-to-csv-or-similar.1350341>
* <http://linuxsleuthing.blogspot.com/2012/10/addressing-ios6-address-book-and-sqlite.html>
