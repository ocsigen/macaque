--
-- PostgreSQL database dump
--

-- Started on 2009-07-30 21:10:34 UTC

SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- TOC entry 1753 (class 1262 OID 16385)
-- Name: base; Type: DATABASE; Schema: -; Owner: -
--

CREATE DATABASE base WITH TEMPLATE = template0 ENCODING = 'SQL_ASCII';

\connect base

SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1471 (class 1259 OID 16395)
-- Dependencies: 3
-- Name: ingredient; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE ingredient (
    id integer NOT NULL,
    nom text
);


--
-- TOC entry 1473 (class 1259 OID 16411)
-- Dependencies: 3
-- Name: liste; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE liste (
    recette integer,
    ingredient integer
);


--
-- TOC entry 1472 (class 1259 OID 16403)
-- Dependencies: 3
-- Name: recette; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE recette (
    id integer NOT NULL,
    nom text
);


--
-- TOC entry 1474 (class 1259 OID 16424)
-- Dependencies: 3
-- Name: test_insert; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_insert (
    foo text,
    bar text
);


--
-- TOC entry 1747 (class 0 OID 16395)
-- Dependencies: 1471
-- Data for Name: ingredient; Type: TABLE DATA; Schema: public; Owner: -
--

COPY ingredient (id, nom) FROM stdin;
0	oeuf
1	lait
2	farine
3	beurre
4	sucre
5	sel
\.


--
-- TOC entry 1749 (class 0 OID 16411)
-- Dependencies: 1473
-- Data for Name: liste; Type: TABLE DATA; Schema: public; Owner: -
--

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


--
-- TOC entry 1748 (class 0 OID 16403)
-- Dependencies: 1472
-- Data for Name: recette; Type: TABLE DATA; Schema: public; Owner: -
--

COPY recette (id, nom) FROM stdin;
0	gateau
1	omelette
2	pain_perdu
\.


--
-- TOC entry 1750 (class 0 OID 16424)
-- Dependencies: 1474
-- Data for Name: test_insert; Type: TABLE DATA; Schema: public; Owner: -
--

COPY test_insert (foo, bar) FROM stdin;
gateau	farine
gateau	sucre
gateau	oeuf
gateau	lait
omelette	sel
omelette	oeuf
pain_perdu	oeuf
pain_perdu	lait
pain_perdu	sucre
\.


--
-- TOC entry 1742 (class 2606 OID 16402)
-- Dependencies: 1471 1471
-- Name: ingredient_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ingredient
    ADD CONSTRAINT ingredient_pkey PRIMARY KEY (id);


--
-- TOC entry 1744 (class 2606 OID 16410)
-- Dependencies: 1472 1472
-- Name: recette_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY recette
    ADD CONSTRAINT recette_pkey PRIMARY KEY (id);


--
-- TOC entry 1746 (class 2606 OID 16419)
-- Dependencies: 1473 1741 1471
-- Name: liste_ingredient_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY liste
    ADD CONSTRAINT liste_ingredient_fkey FOREIGN KEY (ingredient) REFERENCES ingredient(id);


--
-- TOC entry 1745 (class 2606 OID 16414)
-- Dependencies: 1473 1743 1472
-- Name: liste_recette_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY liste
    ADD CONSTRAINT liste_recette_fkey FOREIGN KEY (recette) REFERENCES recette(id);


-- Completed on 2009-07-30 21:10:34 UTC

--
-- PostgreSQL database dump complete
--

