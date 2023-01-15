.read data.sql


CREATE TABLE average_prices AS
  SELECT category AS category, avg(MSRP) AS average_price FROM products GROUP BY category;


CREATE TABLE lowest_prices AS
  SELECT store, item, min(price) FROM inventory GROUP BY item;


CREATE TABLE shopping_list AS
  SELECT a.name, b.store FROM products AS a, lowest_prices AS b 
    WHERE a.name = b.item GROUP BY category HAVING min(MSRP / rating);


CREATE TABLE total_bandwidth AS
  SELECT sum(Mbs) FROM stores AS a, shopping_list AS b
    WHERE a.store = b.store;

