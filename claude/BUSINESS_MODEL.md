# Business Model & Licensing Strategy

Sustainability plan for Cot as an open-source project with commercial potential.

---

## Licensing

### Open Source (Apache 2.0)

Everything a developer needs to build and ship applications:

- Cot compiler (`cot build`, `cot run`, `cot test`, `cot fmt`)
- Standard library (collections, I/O, networking, JSON, encoding, etc.)
- LSP server and editor extensions (VS Code/Cursor, Zed)
- Tree-sitter grammar
- MCP server for AI-assisted development
- Documentation and examples

**Why Apache 2.0:** Business-friendly (companies can use without legal friction), allows proprietary use (encourages adoption), and proven by Go, Kotlin, and Deno.

### Proprietary (Future)

Revenue comes from where code runs in production and where developers work daily — not from the language itself:

- **Cot Cloud** — Managed hosting, serverless deployment, edge compute, auto-scaling. The primary revenue product. Think Vercel for Cot.
- **Enterprise Runtime** — Production-grade optimizations, memory diagnostics, profiling tools, priority support.
- **Pro Tools** — Visual debugger, performance profiler, team collaboration features.

---

## Trademark

"Cot" should be trademarked to protect the brand:
- Forks must rename (prevents confusion)
- "Cot Compatible" certification for third-party tools
- Standard brand protection

### Estimated Costs

| Jurisdiction | Cost | Notes |
|--------------|------|-------|
| US (USPTO) | $250-350 | Per class, TEAS filing |
| EU (EUIPO) | ~$850 | Covers all 27 EU countries |

**Relevant classes:** Class 9 (computer software), Class 42 (SaaS, cloud computing).
**Budget:** ~$1,500-2,500 for US + EU coverage. Start with US, expand as the project grows.

---

## Revenue Timeline

```
Year 1-2 (Adoption):
  Everything open source. Build community. Establish brand.
  Target: 3K+ GitHub stars, 500+ developers, 5-10 companies using Cot.

Year 2-3 (Monetization):
  Launch Cot Cloud (primary revenue).
  Enterprise Runtime licenses.
  Target: First paying customers, $500K+ ARR trajectory.

Year 3+ (Scale):
  Enterprise contracts. Training & certification.
  Expand cloud platform features.
```

---

## Competitive Positioning

Cot is not competing with systems languages (Zig, Rust) or scripting languages (Python, Ruby). The target is the TypeScript + Next.js ecosystem — full-stack web developers who want better performance, real types, and no runtime overhead.

| | TypeScript + Next.js | Go | Rust | **Cot** |
|---|---|---|---|---|
| Full-stack | Yes (ecosystem) | Backend only | Backend only | **Yes (native)** |
| Types | Weak (structural, erasable) | Strong but simple | Strong | **Strong** |
| Compilation | JIT (V8) | Native | Native | **Native + Wasm** |
| Memory | GC (V8) | GC | Manual/borrow | **ARC** |
| Browser | JS (native) | No | wasm-bindgen (bolt-on) | **Wasm (first-class)** |
| Single binary | No (needs Node/Bun) | Yes | Yes | **Yes** |
| Learning curve | Low | Low | High | **Low-medium** |

**Pitch:** Write like TypeScript, compile to native, deploy anywhere. No runtime, no node_modules, no GC pauses. Server and client from the same source.

### Precedents

| Company | Open Source | Revenue Source |
|---------|------------|----------------|
| Vercel | Next.js (MIT) | Cloud hosting ($313M raised) |
| Supabase | Postgres tooling (Apache) | Hosted platform ($116M raised) |
| Deno | Deno runtime (MIT) | Cloud platform ($21M raised) |
| JetBrains | Kotlin (Apache) | IDE subscriptions |

**Key insight:** Open source doesn't prevent value creation — the value is in team expertise, community momentum, and technology lead.

---

## Funding Strategy

### The Reality

VCs don't fund programming languages. They fund businesses. The language is the wedge, Cot Cloud is the product.

### Pre-Funding Requirements

Before approaching investors, Cot needs organic traction:

- GitHub stars: 3,000+
- Monthly active developers: 500+
- Companies using Cot: 5-10
- Community: 500+ Discord members
- External contributors: 10+
- Revenue signal: 1 paid pilot ($5K-20K) or 2-3 LOIs

### Funding Stages

| Stage | Amount | When | Who |
|-------|--------|------|-----|
| Angels / Pre-seed | $100K-500K | At 1.0 launch | Technical angels |
| Seed | $1M-3M | 6-12 months post-launch with traction | Heavybit, Boldstart, YC |
| Series A | $5M-15M | 18-24 months with revenue | a16z, Greylock, Index |

### Bootstrap Alternative (Bun/Zig Model)

```
Stay lean (solo or tiny team)
  + Corporate sponsors ($2K-10K/month each)
  + GitHub Sponsors for individuals
  + Grants (Sovereign Tech Fund: up to EUR 300K, NLNet: up to EUR 50K)
  + Build excellence -> acquisition opportunity
```

**Advantages:** No dilution, no board meetings, full focus on product. Zig sustains a team of ~10 on sponsors alone. Bun built this way and was acquired for ~$1B.

### Recommended Path

```
Now -> 1.0:
  100% product focus. Make the language excellent.

1.0 -> 6 months:
  Public launch. Community building. Conference talks. Blog posts.
  Find 5-10 early adopter companies organically.

6-12 months post-launch:
  Launch Cot Cloud beta. Get 2-3 paying customers (any amount).
  Decide: VC raise vs sponsor/bootstrap based on traction.
```

**Best position:** Not needing to fundraise. Traction creates leverage.

---

## Available Grants

| Source | Amount | Notes |
|--------|--------|-------|
| Sovereign Tech Fund | up to EUR 300K | EU-based, open source infrastructure |
| NLNet Foundation | up to EUR 50K | European, privacy/security focus |
| GitHub Sponsors | Varies | Individual + corporate sponsors |
| Open Collective | Varies | Transparent community funding |

---

*Extracted from pre-0.1 planning docs (`~/cotlang/roadmap/10-business-model.md`), refactored for Cot 0.3+ positioning and architecture.*
