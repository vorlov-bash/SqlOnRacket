#### Example:
```
> select ProductID,ProductName,ProductID from resources/products.csv
> select distinct SupplierID,ProductName,ProductID from resources/products.csv
> select distinct SupplierID from resources/products.csv where(SupplierID>20)
///
> select * from resources/products.csv where(ProductID>20 and SupplierID<10)
> select * from resources/products.csv where(ProductID>20 and SupplierID<10)->orderby(ReorderLevel ASC)
> select count(ProductID) from resources/products.csv (count med sum)
///
> select * from resources/products.csv inner_join(resources/shippers.csv on ProductID=ShipperID)
> select * from resources/products.csv right_join(resources/shippers.csv on ProductID=ShipperID) #добав запис в shippers
> select * from resources/products.csv full_outer_join(resources/shippers.csv on ProductID=ShipperID)
///
> select * from resources/products.csv groupby(SupplierID)->orderby(ReorderLevel)->where(ProductID>60)
> select count(Discontinued),ProductID from resources/products.csv groupby(ProductID)->having(count(Discontinued)=10)
> select case(condition(when Discontinued=0 then "end";when Discontinued=1 then "continued";) end as col),ProductID,Discontinued from resources/products.csv where(ProductID>50)
///
```