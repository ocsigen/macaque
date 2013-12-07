DROP TABLE liste;
DROP TABLE ingredient;
DROP TABLE recette;

DROP SEQUENCE sequence_test_id_seq;
DROP TABLE sequence_test;

DROP TABLE tag;
DROP TABLE url;

DROP TABLE test1;
DROP TABLE test2;

CREATE TABLE test1 (
    text text
);
CREATE TABLE test2 (
    text text
);

CREATE TABLE ingredient (
    id integer NOT NULL,
    nom text
);

ALTER TABLE ONLY ingredient
    ADD CONSTRAINT ingredient_pkey PRIMARY KEY (id);

COPY ingredient (id, nom) FROM stdin;
0	oeuf
1	lait
2	farine
3	beurre
4	sucre
5	sel
\.

CREATE TABLE recette (
    id integer NOT NULL,
    nom text
);

ALTER TABLE ONLY recette
    ADD CONSTRAINT recette_pkey PRIMARY KEY (id);

COPY recette (id, nom) FROM stdin;
0	gateau
1	omelette
2	pain_perdu
\.

CREATE TABLE liste (
    recette integer,
    ingredient integer
);

ALTER TABLE ONLY liste
    ADD CONSTRAINT liste_ingredient_fkey FOREIGN KEY (ingredient) REFERENCES ingredient(id);
ALTER TABLE ONLY liste
    ADD CONSTRAINT liste_recette_fkey FOREIGN KEY (recette) REFERENCES recette(id);

COPY liste (recette, ingredient) FROM stdin;
0	2
0	4
0	0
0	1
1	5
1	0
2	0
2	1
2	4
\.

CREATE TABLE sequence_test (
       id bigint NOT NULL,
       text text
);

CREATE SEQUENCE sequence_test_id_seq;


CREATE TABLE url (
    id integer NOT NULL,
    text text
);

CREATE TABLE tag (
    url_id integer NOT NULL,
    text text
);

ALTER TABLE ONLY url
    ADD CONSTRAINT url_pkey PRIMARY KEY (id);
ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_url_id_fkey FOREIGN KEY (url_id) REFERENCES url(id);

COPY url (id, text) FROM stdin;
0	'http://foo'
1	'http://bar'
2	'http://bla'
3	'http://foo'
\.

COPY tag (url_id, text) FROM stdin;
0	'foo is cool'
0	'foo is great'
1	'bar is also cool'
2	'and bla is the best'
\.
