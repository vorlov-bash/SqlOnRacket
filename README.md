## Usage
```
> load("resources/<filename>")
> select * from resources/<filename>
> select col1,col2,col3,col1 from resources/<filename>
```
#### Example:
```
> select id_event,date_agenda,id_question from resources/plenary_register_mps-skl9.tsv
> select distinct col,id_fr,row,col from resources/map_zal-skl9.csv
> select col,id_fr,row,col from resources/map_zal-skl9.csv where(col=10)
> select column_name(s) from table1/resources.csv INNER_JOIN(table2/resources.csv ON table1.column_name=table.2column_name)
> select column_name(s) from table1/resources.csv FULL_OUTER_JOIN(table2/resources.csv ON table1.column_name=table.2column_name)
> select column_name(s) from table1/resources.csv RIGHT_JOIN(table2/resources.csv ON table1.column_name=table.2column_name)
```