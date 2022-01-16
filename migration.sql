CREATE TABLE IF NOT EXISTS "routine_item"("id" INTEGER PRIMARY KEY,"routine_id" INTEGER NOT NULL REFERENCES "routine" ON DELETE RESTRICT ON UPDATE RESTRICT,"exercise_id" INTEGER NULL REFERENCES "exercise" ON DELETE RESTRICT ON UPDATE RESTRICT,"comment" VARCHAR NOT NULL,"duration_min" INTEGER NOT NULL,"order" INTEGER NOT NULL,CONSTRAINT "unique_exercise_routine_order" UNIQUE ("exercise_id","comment","routine_id","order"));

INSERT INTO "routine_item" (id,routine_id,exercise_id,comment,duration_min,"order")
                     SELECT id,routine_id,exercise_id,''        ,duration_min,"order" FROM routine_exercise;

DROP TABLE routine_exercise;
