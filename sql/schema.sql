-- schema.sql ---------------------------------------------------------------
-- Supabase / PostgREST backing store for 21D-BARRIERS-v2.
--
-- Mirrors the four CSV tables exactly, so switching CFG$store_backend from
-- "csv" to "postgrest" changes where rows land and nothing else.
--
-- SECURITY MODEL
--   * The survey writes with the anon key and is granted INSERT only. It never
--     needs to read, so a leaked anon key cannot be used to pull responses.
--   * The append-only design means there is no UPDATE or DELETE grant at all;
--     a corrected answer is a new row with a higher rev.
--   * The admin panel and the analysis pipeline connect with the service role,
--     which is never exposed to the browser.
--   * No direct identifiers are collected. Postcode is district only and is
--     constrained below so a full postcode cannot be inserted even by mistake.
-- ---------------------------------------------------------------------------

create schema if not exists survey;

create table if not exists survey.respondents (
  id                bigserial primary key,
  rid               text        not null,
  rev               integer     not null,
  ts_utc            timestamptz not null default now(),
  status            text        not null check (status in ('landed','screened_out','partial','complete')),
  path              text        check (path in ('A','B')),
  stage_key         text,
  arm               text        check (arm in ('bws_first','dce_first')),
  modules_served    text,
  age_band          text,
  quota_band        text,
  screen_out_reason text,
  consent           smallint,
  mdas_total        smallint    check (mdas_total between 5 and 25),
  mdas_flag         text,
  dominance_failed  smallint,
  income_mid        numeric,
  seconds_total     numeric,
  page_reached      integer,
  n_pages           integer,
  instrument        text,
  app_version       text,
  design_version    text,
  config            text,
  session           text,
  unique (rid, rev)
);

create table if not exists survey.items (
  id       bigserial primary key,
  rid      text        not null,
  rev      integer     not null,
  ts_utc   timestamptz not null default now(),
  module   text        not null,
  item_id  text        not null,
  value    text,
  seconds  numeric,
  -- Field control: postcode DISTRICT only. Rejects any inward code.
  constraint postcode_district_only check (
    item_id <> 'dem_postcode'
    or value is null
    or value ~ '^[A-Z]{1,2}[0-9][0-9A-Z]?$'
  )
);

create table if not exists survey.dce (
  id             bigserial primary key,
  rid            text        not null,
  rev            integer     not null,
  ts_utc         timestamptz not null default now(),
  task_order     integer     not null,
  set_id         text        not null,
  block          integer,
  dominance      smallint    not null default 0,
  side_flipped   smallint,
  alt            text        not null check (alt in ('A','B')),
  rate           numeric     not null,
  fee            numeric     not null,
  monthly        numeric,
  chosen         smallint    not null check (chosen in (0,1)),
  -- Substitution and induced demand are read off these two fields; they are
  -- never collapsed into a single opt-out indicator.
  stage2_take    text        check (stage2_take in ('Yes','No')),
  stage2_outcome text        check (stage2_outcome in ('take_loan','pay_privately','do_not_proceed')),
  seconds        numeric
);

create table if not exists survey.bws (
  id         bigserial primary key,
  rid        text        not null,
  rev        integer     not null,
  ts_utc     timestamptz not null default now(),
  set_order  integer     not null,
  set_id     text        not null,
  position   integer,
  item_id    text        not null,
  best       smallint    not null check (best in (0,1)),
  worst      smallint    not null check (worst in (0,1)),
  seconds    numeric,
  check (not (best = 1 and worst = 1))
);

create index if not exists respondents_rid_idx on survey.respondents (rid, rev desc);
create index if not exists items_rid_idx        on survey.items       (rid, item_id, rev desc);
create index if not exists dce_rid_idx          on survey.dce         (rid, set_id, alt, rev desc);
create index if not exists bws_rid_idx          on survey.bws         (rid, set_id, item_id, rev desc);

-- Latest-revision views. The analysis pipeline reads these, not the raw tables.
create or replace view survey.v_respondents as
  select distinct on (rid) * from survey.respondents order by rid, rev desc;
create or replace view survey.v_items as
  select distinct on (rid, item_id) * from survey.items order by rid, item_id, rev desc;
create or replace view survey.v_dce as
  select distinct on (rid, set_id, alt) * from survey.dce order by rid, set_id, alt, rev desc;
create or replace view survey.v_bws as
  select distinct on (rid, set_id, item_id) * from survey.bws order by rid, set_id, item_id, rev desc;

-- Row-level security: insert only, for anon and authenticated.
alter table survey.respondents enable row level security;
alter table survey.items       enable row level security;
alter table survey.dce         enable row level security;
alter table survey.bws         enable row level security;

do $$
declare t text;
begin
  foreach t in array array['respondents','items','dce','bws'] loop
    execute format('drop policy if exists %I_insert on survey.%I', t, t);
    execute format('create policy %I_insert on survey.%I for insert to anon, authenticated with check (true)', t, t);
    execute format('grant insert on survey.%I to anon, authenticated', t);
    execute format('grant usage, select on sequence survey.%I_id_seq to anon, authenticated', t);
  end loop;
end $$;

grant usage on schema survey to anon, authenticated;
-- Deliberately NOT granted: select, update, delete.
