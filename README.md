# 라이브러리,프로젝트 이름,핵심 배우는 점,GitHub 바로가기,실행 명령어
1,containers + unordered-containers,containers benchmarks,"IntMap, HashMap, Set 성능 벤치마크 (10GB 데이터 처리 예제)",https://github.com/haskell/containers/tree/master/benchmarks,stack build && stack exec containers-bench
2,text + bytestring,bytestring performance tests,Text vs String vs ByteString 변환/파싱 성능 (70배 차이 실증),https://github.com/haskell/bytestring/tree/master/tests/performance,cabal build && ./dist/build/test/test
3,mtl + transformers,mtl ReaderT examples,ReaderT 패턴으로 환경/설정 관리 (실무 99% 적용 예제),https://github.com/haskell/mtl/tree/master/examples/reader,stack new mtl-ex . && stack run
4,lens + generic-lens,generic-lens modern setup,Generic으로 레코드 렌즈 자동 생성 + JSON 중첩 수정 (보일러플레이트 0),https://github.com/mtamc/generic-lens-modern-setup,stack build && stack exec example

# Lens Library Reference
## Type Name Quotation
The `''` in `makeLenses ''User` is Template Haskell syntax for **type name quotation**.

| Syntax | Meaning | Example |
|--------|---------|---------|
| `''TypeName` | Quote type name | `''User` → references the `User` type |
| `'functionName` | Quote function name | `'map` → references the `map` function |
| `[| expr |]` | Quote expression | `[| x + 1 |]` → quotes the expression |

The `makeLenses ''User` tells Template Haskell to:
1. Look at the `User` data type
2. Find fields starting with `_` (like `_name`, `_address`)
3. Generate lens functions (`name`, `address`) automatically

Without `''`, it would try to find a variable named `User` instead of the type `User`.

## Core Operators
| Operator | Meaning | Example | Type Signature |
|----------|---------|---------|----------------|
| `^.` | view (get) | `alice ^. name → "Alice"` | `s -> Getting a s a -> a` |
| `.~` | set | `name .~ "Bob" $ alice` | `Setter s t a b -> b -> s -> t` |
| `%~` | over (modify) | `name %~ ("Dr. "++) $ alice` | `Setter s t a b -> (a->b) -> s -> t` |
| `+~, *~` | numeric modify | `age +~ 5 $ person` | `Num a => Setter s t a a -> a -> s -> t` |
| `<>~` | Monoid append | `tags <>~ ["haskell"] $ post` | `Monoid a => Setter s t a a -> a -> s -> t` |
| `?~` | Maybe set (Just) | `middleName ?~ "Danger" $ person` | `Setter s t a (Maybe a) -> a -> s -> t` |
| `.` | lens composition | `address . street . streetName` | `Lens s t a b -> Lens a b c d -> Lens s t c d` |

## Key Concepts
| Concept | Description | Usage |
|---------|-------------|-------|
| **Lens** | Bidirectional accessor | Get/set single field |
| **Traversal** | Multiple targets | Modify multiple elements |
| **Prism** | Sum type accessor | Work with Maybe/Either |
| **Iso** | Isomorphism | Convert between types |

## Common Patterns
| Pattern | Code | Result |
|---------|------|--------|
| **Nested Access** | `user ^. address . street . streetName` | Get nested field |
| **Nested Update** | `user & address . street . streetName .~ "New St"` | Update nested field |
| **Multiple Updates** | `user & name .~ "Bob" & age +~ 1` | Chain updates |
| **Conditional Set** | `user & middleName ?~ "X"` | Set Maybe field |
5,aeson,haskell-aeson-examples,복잡 JSON 자동 파싱 + GitHub API 클라이언트 (deriveGeneric 활용),https://github.com/timothyylim/haskell-aeson-examples,stack run -- example-github-client
6,servant,example-servant-minimal,"타입 안전 REST API 서버 (인증 + CRUD, Trello-like 미니 앱)",https://github.com/haskell-servant/example-servant-minimal,stack run → localhost:8080
7,persistent + esqueleto,esqueleto blog examples,타입 안전 DB 쿼리 + 조인/full-text search (블로그 CMS 예제),https://github.com/bitemyapp/esqueleto/tree/master/examples,stack run -- migrate && stack run

8,optparse-applicative,optparse-applicative CLI examples,복잡 CLI 서브커맨드 + GitHub 백업 툴 (bash completion 포함),https://github.com/pcapriotti/optparse-applicative/tree/master/examples,stack run -- backup --user yourusername

 • stack run -- hello (will show "Arguments: hello")
 • stack run -- -v hello (will show verbose mode + "Arguments: hello")
 • stack run -- -i input.txt -o output.txt hello world (will show all options)

9,http-client + http-client-tls,stamina HTTP retry,Exponential backoff + 재시도 로직 (Rate Limit 자동 처리),https://github.com/cachix/stamina.hs,stack run -- retry-example

http-client + http-client-tls: http-client는 HTTP 요청/응답을 처리하며, http-client-tls는 TLS(HTTPS) 지원을 추가합니다. Manager를 생성하여 연결 풀을 관리합니다.
Stamina 라이브러리:
Stamina.retry 또는 Stamina.HTTP.retry: 예외 발생 시 자동 재시도.
RetrySettings: 초기 지연(initialDelay), 백오프 팩터(backoffFactor, 기본 2.0으로 지수 증가), 최대 지연(maxDelay), 최대 시도 횟수(maxAttempts), 총 시간 제한(maxTotalDelay) 설정.
Stamina.HTTP: HTTP 전용으로, HttpException 중 재시도 가능한 것(타임아웃, 5xx 서버 오류, 429 Rate Limit)을 감지. 429 시 Retry-After 헤더를 자동 존중.

Exponential Backoff: 지연 = initialDelay * (backoffFactor ^ attempts) + jitter. Jitter는 랜덤(0~지연 범위)으로 동시 재시도(Thundering Herd) 방지.
Rate Limit 처리: 서버가 429 응답 시 Retry-After 헤더(초 단위 또는 HTTP-date)를 읽어 지연을 오버라이드.

10,async + stm,haskell-web-crawler,1000 concurrent 크롤러 + STM 큐로 중복 제거 (웹 크롤링),https://github.com/jordanspooner/haskell-web-crawler,stack run -- https://news.ycombinator.com
11,vector,kmeans-vector,"고성능 K-Means 클러스터링 (100만 점 0.8초 처리, unboxed vector)",https://github.com/alpmestan/kmeans-vector,stack run -- 1000000
12,relude,protolude (relude-like template),"현대적 Prelude 템플릿 (에러 핸들링 + 안전 함수, 모든 프로젝트 적용)",https://github.com/sdiebert/protolude,stack new relude-template . && stack run
13,exceptions + mtl,io-region bracket examples,bracket + MonadMask로 안전 자원 관리 (async 예외 처리),https://github.com/Yuras/io-region/tree/master/examples,stack run -- bracket-test
14,QuickCheck + Hedgehog,hedgehog-classes aeson laws,속성 기반 테스트 (Aeson roundtrip 법칙 100% 검증),https://github.com/hedgehogqa/haskell-hedgehog-classes/tree/master/test,stack test
15,generic-optics + optics,rapid hot-reload daemon,Optics로 nested config 실시간 갱신 (hot reload 데몬),https://github.com/Yuras/rapid,stack run
