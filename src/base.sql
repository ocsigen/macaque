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