# =====================================================================
# PERAMALAN JUMLAH PERJALANAN WISATAWAN NUSANTARA PROVINSI BANTEN 2026
# Skrip final (mandiri, satu file)
# ---------------------------------------------------------------------
# Model terpilih : SARIMA(0,1,2)(1,0,1)[12] + drift pada log perjalanan
# Pembanding     : SARIMAX Lebaran Exposure H+-10 (log) dan metode naif
# Validasi       : rolling origin, titik awal Des 2024 - Nov 2025, target <= Des 2025
# Uji            : latih Jan 2021 - Des 2025, uji Jan - Sep 2026 (aktual tersedia)
# Ramalan        : Jan - Des 2026 (12 bulan) beserta interval prediksi 80% dan 95%
#
# Pemilihan model lengkap (16 varian SARIMA/SARIMAX, dummy 2025, uji structural
# break) ada di Analisis_Lanjutan_SARIMAX_Lebaran_Break_2026.R. Skrip ini hanya
# menjalankan model final dan dua pembanding untuk presentasi.
#
# Cara pakai: buka di RStudio, sesuaikan folder_proyek bila perlu, lalu Source.
# Waktu jalan +- 1 menit. Paket yang dibutuhkan: forecast.
# =====================================================================

# 0. KONFIGURASI -------------------------------------------------------
folder_proyek <- "C:/2025 coding/Kuliah/Semester 5/Data Challage/presentasi new"
data_dir <- file.path(folder_proyek, "data")
output_dir <- file.path(folder_proyek, "hasil_final_2026")
grafik_dir <- file.path(output_dir, "grafik_slide")
dir.create(grafik_dir, recursive = TRUE, showWarnings = FALSE)

lib_proyek <- file.path(folder_proyek, ".R-library")
if (dir.exists(lib_proyek)) .libPaths(c(lib_proyek, .libPaths()))
if (!requireNamespace("forecast", quietly = TRUE)) {
  stop("Paket forecast belum tersedia. Jalankan install.packages('forecast').")
}

daftar_tahun <- 2021:2026
prefix_file <- paste0("Jumlah Perjalanan Wisatawan Nusantara Menurut ",
                      "Kabupaten_Kota Tujuan di Provinsi Banten, ")
nama_total <- "Provinsi Banten"
akhir_latih <- as.Date("2025-12-01")
awal_rolling <- as.Date("2024-12-01") # orde SARIMA dibekukan dengan data s.d. Des 2024
horizon <- 12L
jendela_lebaran <- 10 # H-10 s.d. H+10

# Tanggal 1 Syawal menurut sidang isbat pemerintah (sumber: laporan penelitian).
idulfitri <- as.Date(c("2021-05-13", "2022-05-02", "2023-04-22",
                       "2024-04-10", "2025-03-31", "2026-03-21"))

bulan_id <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli",
              "Agustus", "September", "Oktober", "November", "Desember")
bln <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des")

# Warna grafik (selaras dengan template presentasi).
warna <- c(navy = "#1F3864", merah = "#8B2F35", emas = "#D99A00",
           hijau = "#2E7D7A", abu = "#8C8C8C", abu_muda = "#D9D9D9")

simpan_csv <- function(df, nama) {
  write.csv(df, file.path(output_dir, nama), row.names = FALSE, na = "")
}
persen <- function(a, b) 100 * (a / b - 1)

# 1. IMPOR DAN VALIDASI DATA -------------------------------------------------
baca_tahun <- function(tahun) {
  f <- file.path(data_dir, paste0(prefix_file, tahun, ".csv"))
  isi <- read.csv(f, skip = 3, header = TRUE, check.names = FALSE,
                  colClasses = "character", na.strings = c("", "-"),
                  fileEncoding = "UTF-8-BOM")
  names(isi)[1] <- "wilayah"
  do.call(rbind, lapply(1:12, function(b) {
    teks <- trimws(isi[[b + 1]])
    nilai <- suppressWarnings(as.numeric(teks))
    stopifnot(!any(!is.na(teks) & is.na(nilai)))
    data.frame(tanggal = as.Date(sprintf("%d-%02d-01", tahun, b)), tahun = tahun,
               bulan = b, wilayah = trimws(isi$wilayah), perjalanan = nilai)
  }))
}
data_long <- do.call(rbind, lapply(daftar_tahun, baca_tahun))
data_long <- data_long[order(data_long$tanggal, data_long$wilayah), ]
stopifnot(anyDuplicated(data_long[, c("tanggal", "wilayah")]) == 0,
          length(unique(data_long$wilayah)) == 9,
          all(data_long$perjalanan >= 0, na.rm = TRUE))
wilayah_kab <- setdiff(sort(unique(data_long$wilayah)), nama_total)

# Total provinsi BPS vs jumlah delapan kabupaten/kota.
prov <- subset(data_long, wilayah == nama_total & !is.na(perjalanan))
prov <- prov[order(prov$tanggal), ]
jumlah_kab <- sapply(prov$tanggal, function(t) {
  sum(data_long$perjalanan[data_long$tanggal == t & data_long$wilayah != nama_total])
})
selisih_total <- data.frame(tanggal = prov$tanggal, total_bps = prov$perjalanan,
                            jumlah_8_wilayah = jumlah_kab,
                            selisih = prov$perjalanan - jumlah_kab)
anomali <- subset(selisih_total, selisih != 0)
simpan_csv(selisih_total, "01_validasi_total_vs_8_wilayah.csv")
cat("Bulan tidak konsisten:", nrow(anomali), "\n")
print(anomali, row.names = FALSE)

# Kab Pandeglang Juli 2025 tercatat 307 (diduga salah satuan). CSV tidak diubah.
# Seri model memakai total provinsi BPS sehingga tidak terpengaruh. Nilai
# rekonsiliasi (total - 7 wilayah lain) hanya dipakai untuk grafik kontribusi wilayah.
data_wilayah <- data_long
for (i in seq_len(nrow(anomali))) {
  t <- anomali$tanggal[i]
  baris <- data_wilayah$tanggal == t & data_wilayah$wilayah == "Kab Pandeglang"
  data_wilayah$perjalanan[baris] <- data_wilayah$perjalanan[baris] + anomali$selisih[i]
}

N <- nrow(prov)
y <- ts(prov$perjalanan, start = c(2021, 1), frequency = 12)
k_latih <- match(akhir_latih, prov$tanggal)
k_rolling <- match(awal_rolling, prov$tanggal)
stopifnot(identical(prov$tanggal, seq(as.Date("2021-01-01"), by = "month", length.out = N)),
          k_latih == 60L)
cat("Data provinsi:", format(prov$tanggal[1], "%b %Y"), "-",
    format(prov$tanggal[N], "%b %Y"), "(", N, "bulan )\n")

# 2. RINGKASAN DATA -------------------------------------------------------------
tahunan <- aggregate(perjalanan ~ tahun, data = prov, FUN = sum)
tahunan$bulan_tersedia <- as.vector(table(prov$tahun))
tahunan$pertumbuhan_persen <- c(NA, persen(tahunan$perjalanan[-1], head(tahunan$perjalanan, -1)))
tahunan$pertumbuhan_persen[tahunan$bulan_tersedia < 12] <- NA
# 2026 belum lengkap: bandingkan bulan yang sama.
th_parsial <- tahunan$tahun[tahunan$bulan_tersedia < 12]
for (th in th_parsial) {
  b <- prov$bulan[prov$tahun == th]
  tahunan$pertumbuhan_bulan_sama[tahunan$tahun == th] <- persen(
    sum(prov$perjalanan[prov$tahun == th]),
    sum(prov$perjalanan[prov$tahun == th - 1 & prov$bulan %in% b]))
}
simpan_csv(tahunan, "02_total_tahunan.csv")

y24 <- prov$perjalanan[prov$tahun == 2024]
y25 <- prov$perjalanan[prov$tahun == 2025]
yoy_2025 <- persen(y25, y24)

kontribusi <- do.call(rbind, lapply(wilayah_kab, function(w) {
  a <- sum(data_wilayah$perjalanan[data_wilayah$wilayah == w & data_wilayah$tahun == 2025])
  b <- sum(data_wilayah$perjalanan[data_wilayah$wilayah == w & data_wilayah$tahun == 2024])
  data.frame(wilayah = w, tahun_2024 = b, tahun_2025 = a, kenaikan = a - b,
             pertumbuhan_persen = persen(a, b))
}))
kontribusi$porsi_kenaikan_persen <- 100 * kontribusi$kenaikan / sum(kontribusi$kenaikan)
kontribusi <- kontribusi[order(-kontribusi$kenaikan), ]
simpan_csv(kontribusi, "03_kontribusi_wilayah_2025.csv")

# 3. LEBARAN EXPOSURE ---------------------------------------------------------------
# Banyak hari dalam bulan yang termasuk jendela H-10 s.d. H+10 di sekitar 1 Syawal.
bulan_kal <- seq(as.Date("2021-01-01"), as.Date("2026-12-01"), by = "month")
hari_lebaran <- do.call(c, lapply(idulfitri, function(t) {
  seq(t - jendela_lebaran, t + jendela_lebaran, by = "day")
}))
lebaran <- sapply(seq_along(bulan_kal), function(i) {
  akhir <- seq(bulan_kal[i], by = "month", length.out = 2)[2]
  sum(hari_lebaran >= bulan_kal[i] & hari_lebaran < akhir)
})
stopifnot(sum(lebaran) == length(idulfitri) * (2 * jendela_lebaran + 1))
# Untuk penanda grafik: bulan yang memuat sebagian besar pekan sesudah Idulfitri (H s.d. H+7).
bulan_lebaran <- as.Date(format(idulfitri + 3, "%Y-%m-01"))

# 3b. UJI STASIONERITAS (DASAR PEMILIHAN d = 1, D = 0) -------------------------------
# Diuji pada log perjalanan data latih (Jan 2021 - Des 2025), sama dengan seri model.
if (!requireNamespace("urca", quietly = TRUE)) {
  stop("Paket urca belum tersedia. Jalankan install.packages('urca').")
}
z_latih <- log(window(y, end = time(y)[k_latih]))
uji_adf <- function(x, tipe, ket) {
  u <- urca::ur.df(x, type = tipe, selectlags = "AIC")
  stat <- unname(u@teststat[1]); kritis <- unname(u@cval[1, "5pct"])
  data.frame(uji = "ADF (H0: ada akar unit)", seri = ket, statistik = stat, kritis_5persen = kritis,
             kesimpulan = if (stat < kritis) "stasioner" else "tidak stasioner")
}
uji_kpss <- function(x, tipe, ket) {
  u <- urca::ur.kpss(x, type = tipe, lags = "short")
  stat <- unname(u@teststat); kritis <- unname(u@cval[1, "5pct"])
  data.frame(uji = "KPSS (H0: stasioner)", seri = ket, statistik = stat, kritis_5persen = kritis,
             kesimpulan = if (stat > kritis) "tidak stasioner" else "stasioner")
}
uji_stasioner <- rbind(
  uji_adf(z_latih, "drift", "log level, tanpa tren"),
  uji_kpss(z_latih, "mu", "log level, tanpa tren"),
  uji_adf(z_latih, "trend", "log level, dengan tren"),
  uji_kpss(z_latih, "tau", "log level, dengan tren"),
  uji_adf(diff(z_latih), "drift", "log, differencing 1 kali"),
  uji_kpss(diff(z_latih), "mu", "log, differencing 1 kali")
)
jumlah_diff <- data.frame(
  fungsi = c("ndiffs (KPSS)", "ndiffs (ADF)", "ndiffs (PP)", "nsdiffs (seas)", "nsdiffs (OCSB)"),
  jumlah_differencing = c(forecast::ndiffs(z_latih, test = "kpss"),
                          forecast::ndiffs(z_latih, test = "adf"),
                          forecast::ndiffs(z_latih, test = "pp"),
                          forecast::nsdiffs(z_latih, test = "seas"),
                          forecast::nsdiffs(z_latih, test = "ocsb")))
simpan_csv(uji_stasioner, "12_uji_stasioneritas.csv")
simpan_csv(jumlah_diff, "13_jumlah_differencing.csv")
cat("\n=== UJI STASIONERITAS (log, data latih) ===\n")
print(uji_stasioner, row.names = FALSE, digits = 4)
print(jumlah_diff, row.names = FALSE)
# Catatan: tanpa tren, level tidak stasioner dan stasioner setelah 1 kali differencing
# (d = 1); nsdiffs = 0 (D = 0). Dengan tren linear, seri tampak stasioner di sekitar
# tren; d = 1 + drift tetap dipakai karena tidak memaksa tren deterministik.

# 4. MODEL ---------------------------------------------------------------------------
label_model <- c(
  SARIMA_log = "SARIMA(0,1,2)(1,0,1)[12] + drift (log)",
  SARIMAX_Lebaran = "SARIMAX Lebaran H\u00b110 (log)",
  Naif = "Naif"
)
ramal <- function(model, y_latih, h) {
  k <- length(y_latih)
  lv <- c(80, 95)
  if (model == "Naif") {
    fc <- forecast::naive(y_latih, h = h, level = lv)
    fit <- NULL
  } else {
    argumen <- list(y = y_latih, order = c(0, 1, 2), include.constant = TRUE, lambda = 0,
                    seasonal = list(order = c(1, 0, 1), period = 12))
    if (model == "SARIMAX_Lebaran") argumen$xreg <- cbind(lebaran = lebaran[seq_len(k)])
    # do.call menyimpan nilai (bukan nama variabel) di call; dibutuhkan predict.Arima.
    fit <- tryCatch(do.call(forecast::Arima, argumen),
                    error = function(e) do.call(forecast::Arima, c(argumen, method = "ML")))
    fc <- if (model == "SARIMAX_Lebaran") {
      forecast::forecast(fit, xreg = cbind(lebaran = lebaran[k + seq_len(h)]), level = lv)
    } else forecast::forecast(fit, h = h, level = lv)
  }
  list(fit = fit, fc = fc,
       hasil = data.frame(prediksi = as.numeric(fc$mean),
                          bawah_80 = as.numeric(fc$lower[, 1]), atas_80 = as.numeric(fc$upper[, 1]),
                          bawah_95 = as.numeric(fc$lower[, 2]), atas_95 = as.numeric(fc$upper[, 2])))
}
metrik <- function(aktual, prediksi, skala) {
  e <- aktual - prediksi
  c(MAE = mean(abs(e)), RMSE = sqrt(mean(e^2)), MAPE = 100 * mean(abs(e / aktual)),
    MASE = mean(abs(e) / skala))
}
# Skala MASE = MAE naif musiman (lag 12) pada data latih.
skala_mase <- function(y_latih) mean(abs(diff(as.numeric(y_latih), lag = 12)))

# 5. VALIDASI ROLLING ORIGIN (DASAR PEMILIHAN MODEL) -------------------------------------
asal <- k_rolling:(k_latih - 1L)
rolling <- do.call(rbind, lapply(asal, function(k) {
  y_latih <- window(y, end = time(y)[k])
  h <- min(horizon, k_latih - k) # target tidak melewati Des 2025
  do.call(rbind, lapply(names(label_model), function(m) {
    r <- suppressWarnings(ramal(m, y_latih, h))
    data.frame(model = m, asal = prov$tanggal[k], h = seq_len(h),
               target = prov$tanggal[k + seq_len(h)],
               aktual = as.numeric(y[k + seq_len(h)]), prediksi = r$hasil$prediksi,
               skala = skala_mase(y_latih))
  }))
}))
simpan_csv(rolling, "04_rolling_origin_detail.csv")
tabel_rolling <- do.call(rbind, lapply(names(label_model), function(m) {
  d <- rolling[rolling$model == m, ]
  e <- d$aktual - d$prediksi
  data.frame(model = label_model[[m]], kode = m, n_ramalan = nrow(d),
             MAE = mean(abs(e)), RMSE = sqrt(mean(e^2)),
             MAPE = 100 * mean(abs(e / d$aktual)), MASE = mean(abs(e) / d$skala))
}))
tabel_rolling <- tabel_rolling[order(tabel_rolling$MASE), ]
model_terbaik <- tabel_rolling$kode[1]
simpan_csv(tabel_rolling, "05_evaluasi_rolling_origin.csv")
cat("\n=== VALIDASI ROLLING ORIGIN (", length(asal), "titik awal ) ===\n")
print(tabel_rolling, row.names = FALSE, digits = 4)
cat("Model terbaik:", label_model[[model_terbaik]], "\n")

# 6. UJI JAN - SEP 2026 DAN RAMALAN JAN - DES 2026 -----------------------------------------
y_latih <- window(y, end = time(y)[k_latih])
bulan_ramal <- seq(as.Date("2026-01-01"), by = "month", length.out = horizon)
aktual_2026 <- prov$perjalanan[match(bulan_ramal, prov$tanggal)]
ada <- !is.na(aktual_2026)
hasil <- lapply(setNames(names(label_model), names(label_model)), function(m) {
  suppressWarnings(ramal(m, y_latih, horizon))
})
tabel_uji <- do.call(rbind, lapply(names(label_model), function(m) {
  mt <- metrik(aktual_2026[ada], hasil[[m]]$hasil$prediksi[ada], skala_mase(y_latih))
  data.frame(model = label_model[[m]], kode = m, t(mt),
             galat_maret_persen = 100 * (aktual_2026[3] - hasil[[m]]$hasil$prediksi[3]) / aktual_2026[3],
             total_ramalan_2026 = sum(hasil[[m]]$hasil$prediksi))
}))
simpan_csv(tabel_uji, "06_evaluasi_uji_jan_sep_2026.csv")
cat("\n=== UJI JAN - SEP 2026 ===\n")
print(tabel_uji, row.names = FALSE, digits = 4)

ramalan <- data.frame(tanggal = bulan_ramal, bulan = bulan_id,
                      round(hasil[[model_terbaik]]$hasil), aktual = aktual_2026)
ramalan$selisih <- ramalan$aktual - ramalan$prediksi
ramalan$galat_persen <- 100 * abs(ramalan$selisih) / ramalan$aktual
ramalan$dalam_interval_95 <- ramalan$aktual >= ramalan$bawah_95 & ramalan$aktual <= ramalan$atas_95
simpan_csv(ramalan, "07_ramalan_2026_model_terbaik.csv")
semua_ramalan <- data.frame(tanggal = bulan_ramal, aktual = aktual_2026,
                            sapply(hasil, function(r) round(r$hasil$prediksi)))
simpan_csv(semua_ramalan, "08_ramalan_2026_tiga_model.csv")
cat("\n=== RAMALAN 2026:", label_model[[model_terbaik]], "===\n")
print(ramalan[, -1], row.names = FALSE)
cat("Total ramalan 2026:", sum(ramalan$prediksi), "\n")

# Diagnostik model terbaik (data latih Jan 2021 - Des 2025).
fit_terbaik <- hasil[[model_terbaik]]$fit
if (!is.null(fit_terbaik)) {
  se <- sqrt(diag(fit_terbaik$var.coef))
  koef <- data.frame(parameter = names(coef(fit_terbaik)), estimasi = coef(fit_terbaik),
                     galat_baku = se, z = coef(fit_terbaik) / se,
                     p = 2 * pnorm(-abs(coef(fit_terbaik) / se)), row.names = NULL)
  lb <- Box.test(residuals(fit_terbaik), lag = 24, type = "Ljung-Box", fitdf = 4)
  diagnostik <- data.frame(AICc = fit_terbaik$aicc, BIC = fit_terbaik$bic,
                           ljung_box_lag24 = unname(lb$statistic), p_ljung_box = lb$p.value)
  simpan_csv(koef, "09_koefisien_model_terbaik.csv")
  simpan_csv(diagnostik, "10_diagnostik_model_terbaik.csv")
  cat("\n=== KOEFISIEN DAN DIAGNOSTIK ===\n")
  print(koef, row.names = FALSE, digits = 4)
  print(diagnostik, row.names = FALSE, digits = 4)
}

# Pembaruan: latih ulang dengan seluruh data (s.d. bulan terakhir), ramal sisa 2026.
h_sisa <- 12L - as.integer(format(prov$tanggal[N], "%m"))
if (h_sisa > 0) {
  baru <- suppressWarnings(ramal(model_terbaik, y, h_sisa))
  pembaruan <- data.frame(tanggal = seq(prov$tanggal[N], by = "month", length.out = h_sisa + 1)[-1],
                          round(baru$hasil))
  simpan_csv(pembaruan, "11_ramalan_pembaruan_sisa_2026.csv")
  cat("\n=== PEMBARUAN SISA 2026 (latih s.d.", format(prov$tanggal[N], "%b %Y"), ") ===\n")
  print(pembaruan, row.names = FALSE)
}

# 7. GRAFIK UNTUK SLIDE (PNG 16:9, siap tempel di Canva) ---------------------------------
mulai_png <- function(nama, w = 1800, h = 900) {
  png(file.path(grafik_dir, nama), width = w, height = h, res = 180, family = "sans")
  par(mar = c(3.2, 4.6, 1.2, 1), mgp = c(2.6, 0.7, 0), las = 1, bty = "n",
      col.axis = "#404040", col.lab = "#404040", cex.axis = 0.9)
}
sumbu_juta <- function(rentang) {
  at <- pretty(rentang)
  axis(2, at = at, labels = format(at / 1e6, decimal.mark = ","), col = NA, col.ticks = NA)
  abline(h = at, col = "#EDEDED")
}

# G1. Seri waktu 2021 - Sep 2026 dengan periode 2025 dan bulan Lebaran.
mulai_png("G1_tren_2021_2026.png")
rentang <- range(prov$perjalanan)
plot(prov$tanggal, prov$perjalanan, type = "n", axes = FALSE, xlab = "",
     ylab = "Juta perjalanan", ylim = rentang)
rect(as.Date("2025-01-01"), rentang[1] - 1e6, as.Date("2025-12-31"), rentang[2] + 1e6,
     col = "#F6EDE1", border = NA)
sumbu_juta(rentang)
axis(1, at = as.Date(paste0(2021:2026, "-01-01")), labels = 2021:2026, col = NA, col.ticks = NA)
lines(prov$tanggal, prov$perjalanan, lwd = 2.5, col = warna[["navy"]])
leb_bulan <- prov$tanggal %in% bulan_lebaran
points(prov$tanggal[leb_bulan], prov$perjalanan[leb_bulan], pch = 21, cex = 1.6,
       bg = warna[["emas"]], col = "white", lwd = 1.5)
text(as.Date("2025-07-01"), rentang[1] + 0.03 * diff(rentang),
     sprintf("2025: +%s%%", format(round(tahunan$pertumbuhan_persen[tahunan$tahun == 2025], 2),
                                  decimal.mark = ",")),
     col = warna[["merah"]], font = 2, cex = 1)
legend("topleft", c("Perjalanan bulanan", "Bulan Lebaran"), lwd = c(2.5, NA),
       pch = c(NA, 21), pt.bg = c(NA, warna[["emas"]]), col = c(warna[["navy"]], "white"),
       pt.cex = 1.6, bty = "n", cex = 0.9, inset = c(0.01, 0))
dev.off()

# G2. Kontribusi wilayah terhadap kenaikan 2025.
mulai_png("G2_kontribusi_wilayah_2025.png")
par(mar = c(3.6, 12.5, 1.2, 1))
d <- kontribusi[nrow(kontribusi):1, ]
b <- barplot(d$porsi_kenaikan_persen, horiz = TRUE, names.arg = d$wilayah, las = 1, border = NA,
             col = ifelse(rank(-d$porsi_kenaikan_persen) <= 3, warna[["navy"]], "#9DB0CF"),
             xlim = c(0, max(d$porsi_kenaikan_persen) * 1.6), axes = FALSE,
             xlab = "", cex.names = 0.95)
mtext("Porsi dari total kenaikan 2025 (%)  |  dalam kurung: pertumbuhan wilayah 2025 vs 2024",
      side = 1, line = 1.2, cex = 0.85, col = "#404040")
text(d$porsi_kenaikan_persen, b, sprintf("%s%%  (tumbuh +%s%%)",
                                          format(round(d$porsi_kenaikan_persen, 1), decimal.mark = ","),
                                          format(round(d$pertumbuhan_persen, 1), decimal.mark = ",")),
     pos = 4, cex = 0.85, col = "#404040")
dev.off()

# G3. Pola bulanan per tahun: puncak mengikuti Lebaran.
mulai_png("G3_puncak_mengikuti_lebaran.png")
tahun_g3 <- 2023:2026
warna_g3 <- c("#B7C3D9", "#6F86B3", warna[["navy"]], warna[["merah"]])
rentang <- range(prov$perjalanan[prov$tahun %in% tahun_g3])
plot(1:12, type = "n", xlim = c(1, 12.6), ylim = rentang, axes = FALSE, xlab = "",
     ylab = "Juta perjalanan")
sumbu_juta(rentang)
axis(1, at = 1:12, labels = bln, col = NA, col.ticks = NA)
posisi_label <- numeric(0)
for (i in seq_along(tahun_g3)) {
  s <- prov[prov$tahun == tahun_g3[i], ]
  lines(s$bulan, s$perjalanan, lwd = 2.5, col = warna_g3[i])
  idx <- which(s$tanggal %in% bulan_lebaran)
  points(s$bulan[idx], s$perjalanan[idx], pch = 21, cex = 1.8, bg = warna[["emas"]], col = "white")
  # Label tahun lengkap ada di kanan (Des); geser bila terlalu dekat dengan label lain.
  y_lab <- tail(s$perjalanan, 1)
  if (nrow(s) == 12) {
    jarak_min <- 0.05 * diff(rentang)
    while (any(abs(posisi_label - y_lab) < jarak_min)) y_lab <- y_lab - jarak_min
    posisi_label <- c(posisi_label, y_lab)
  }
  text(max(s$bulan) + 0.15, y_lab, tahun_g3[i], col = warna_g3[i], font = 2, adj = 0, cex = 0.9)
}
legend("topright", "Bulan Lebaran (pekan sesudah Idulfitri)", pch = 21, pt.bg = warna[["emas"]],
       col = "white", pt.cex = 1.8, bty = "n", cex = 0.9)
dev.off()

# G4. Uji Jan - Sep 2026: aktual vs tiga model.
mulai_png("G4_uji_2026_tiga_model.png")
warna_model <- c(SARIMA_log = warna[["merah"]], SARIMAX_Lebaran = warna[["hijau"]], Naif = warna[["abu"]])
lty_model <- c(SARIMA_log = 1, SARIMAX_Lebaran = 2, Naif = 3)
rentang <- range(c(aktual_2026, sapply(hasil, function(r) r$hasil$prediksi)), na.rm = TRUE)
rentang[2] <- rentang[2] + 0.3 * diff(rentang) # ruang legenda di atas data
plot(1:12, type = "n", ylim = rentang, axes = FALSE, xlab = "", ylab = "Juta perjalanan")
sumbu_juta(rentang)
axis(1, at = 1:12, labels = bln, col = NA, col.ticks = NA)
rect(9.5, rentang[1] - 1e6, 12.5, rentang[2] + 1e6, col = "#F4F4F4", border = NA)
text(11, rentang[1], "aktual belum tersedia", col = "#8C8C8C", cex = 0.8)
for (m in names(label_model)) {
  lines(1:12, hasil[[m]]$hasil$prediksi, lwd = 2.5, col = warna_model[[m]], lty = lty_model[[m]])
}
points(which(ada), aktual_2026[ada], pch = 19, cex = 1.4, col = warna[["navy"]])
legend("topright", c("Aktual", label_model), pch = c(19, NA, NA, NA), lwd = c(NA, 2.5, 2.5, 2.5),
       lty = c(NA, lty_model), col = c(warna[["navy"]], warna_model), bty = "n", cex = 0.85,
       seg.len = 2.5)
dev.off()

# G5. Ramalan 2026 model terbaik dengan interval prediksi.
mulai_png("G5_ramalan_2026.png")
r <- hasil[[model_terbaik]]$hasil
x_hist <- prov$tanggal[prov$tanggal >= as.Date("2023-01-01")]
y_hist <- prov$perjalanan[prov$tanggal >= as.Date("2023-01-01")]
rentang <- range(c(y_hist, r$bawah_95, r$atas_95))
plot(range(c(x_hist, bulan_ramal)), rentang, type = "n", axes = FALSE, xlab = "",
     ylab = "Juta perjalanan")
sumbu_juta(rentang)
axis(1, at = as.Date(paste0(2023:2026, "-01-01")), labels = 2023:2026, col = NA, col.ticks = NA)
polygon(c(bulan_ramal, rev(bulan_ramal)), c(r$bawah_95, rev(r$atas_95)),
        col = adjustcolor(warna[["merah"]], 0.12), border = NA)
polygon(c(bulan_ramal, rev(bulan_ramal)), c(r$bawah_80, rev(r$atas_80)),
        col = adjustcolor(warna[["merah"]], 0.22), border = NA)
lines(x_hist[x_hist <= akhir_latih], y_hist[x_hist <= akhir_latih], lwd = 2.5, col = warna[["navy"]])
lines(c(akhir_latih, bulan_ramal), c(y[k_latih], r$prediksi), lwd = 2.5, lty = 2, col = warna[["merah"]])
points(bulan_ramal[ada], aktual_2026[ada], pch = 19, cex = 1.2, col = warna[["navy"]])
abline(v = akhir_latih, lty = 3, col = "#8C8C8C")
legend("topleft", c("Aktual", "Ramalan 2026", "Interval 80%", "Interval 95%"),
       lwd = c(2.5, 2.5, NA, NA), lty = c(1, 2, NA, NA), pch = c(19, NA, 15, 15), pt.cex = c(1.2, 1, 2.2, 2.2),
       col = c(warna[["navy"]], warna[["merah"]], adjustcolor(warna[["merah"]], 0.45),
               adjustcolor(warna[["merah"]], 0.2)), bty = "n", cex = 0.85)
dev.off()

# G6. Diagnostik residual model terbaik.
if (!is.null(fit_terbaik)) {
  mulai_png("G6_residual_model_terbaik.png", w = 1800, h = 800)
  par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 1))
  res <- residuals(fit_terbaik)
  plot(prov$tanggal[seq_len(k_latih)], as.numeric(res), type = "h", lwd = 3, col = warna[["navy"]],
       axes = FALSE, xlab = "", ylab = "Residual (log)", main = "Residual", font.main = 1, cex.main = 1)
  axis(1, at = as.Date(paste0(2021:2026, "-01-01")), labels = 2021:2026, col = NA, col.ticks = NA)
  axis(2, col = NA, col.ticks = NA)
  abline(h = 0, col = "#8C8C8C")
  a <- acf(as.numeric(res), lag.max = 24, plot = FALSE)
  batas <- qnorm(0.975) / sqrt(length(res))
  plot(a$lag[-1], a$acf[-1], type = "h", lwd = 4, col = warna[["navy"]], ylim = c(-0.5, 0.5),
       axes = FALSE, xlab = "Lag (bulan)", ylab = "ACF", main = "Autokorelasi residual",
       font.main = 1, cex.main = 1)
  axis(1, at = c(1, 6, 12, 18, 24), col = NA, col.ticks = NA)
  axis(2, col = NA, col.ticks = NA)
  abline(h = c(-batas, batas), lty = 2, col = warna[["merah"]])
  abline(h = 0, col = "#8C8C8C")
  dev.off()
}

# G7. Hasil differencing: log level vs differencing 1 kali, beserta ACF masing-masing.
mulai_png("G7_differencing.png", w = 1800, h = 1100)
par(mfrow = c(2, 2), mar = c(3.4, 4.4, 2.4, 1))
t_latih <- prov$tanggal[seq_len(k_latih)]
dz_latih <- diff(z_latih)
ringkas_uji <- function(ket) {
  u <- uji_stasioner[uji_stasioner$seri == ket, ]
  sprintf("ADF %s  |  KPSS %s", format(round(u$statistik[1], 2), decimal.mark = ","),
          format(round(u$statistik[2], 3), decimal.mark = ","))
}
panel_seri <- function(x, nilai, judul, ylab, garis_nol = FALSE) {
  plot(x, nilai, type = "l", lwd = 2.2, col = warna[["navy"]], axes = FALSE, xlab = "",
       ylab = ylab, main = judul, font.main = 2, cex.main = 1)
  axis(1, at = as.Date(paste0(2021:2026, "-01-01")), labels = 2021:2026, col = NA, col.ticks = NA)
  axis(2, at = axTicks(2), labels = format(axTicks(2), decimal.mark = ","), col = NA, col.ticks = NA)
  if (garis_nol) abline(h = mean(nilai), lty = 2, col = warna[["merah"]])
}
panel_acf <- function(nilai, judul) {
  a <- acf(nilai, lag.max = 24, plot = FALSE)
  batas <- qnorm(0.975) / sqrt(length(nilai))
  plot(a$lag[-1], a$acf[-1], type = "h", lwd = 4, col = warna[["navy"]], ylim = c(-0.6, 1),
       axes = FALSE, xlab = "", ylab = "ACF", main = judul, font.main = 1, cex.main = 0.95)
  axis(1, at = c(1, 6, 12, 18, 24), col = NA, col.ticks = NA)
  mtext("Lag (bulan)", side = 1, line = 2.2, cex = 0.75, col = "#404040")
  axis(2, at = axTicks(2), labels = format(axTicks(2), decimal.mark = ","), col = NA, col.ticks = NA)
  abline(h = c(-batas, batas), lty = 2, col = warna[["merah"]])
  abline(h = 0, col = "#8C8C8C")
}
panel_seri(t_latih, as.numeric(z_latih), "Sebelum: log perjalanan (tidak stasioner)", "ln(perjalanan)")
panel_seri(t_latih[-1], as.numeric(dz_latih), "Sesudah: differencing 1 kali (stasioner)",
           "Δ ln(perjalanan)", garis_nol = TRUE)
panel_acf(as.numeric(z_latih), paste("ACF turun lambat  |", ringkas_uji("log level, tanpa tren")))
panel_acf(as.numeric(dz_latih), paste("ACF cepat hilang  |", ringkas_uji("log, differencing 1 kali")))
dev.off()

writeLines(capture.output(sessionInfo()), file.path(output_dir, "sesi_R.txt"))
cat("\nSelesai. Tabel di", output_dir, "| grafik di", grafik_dir, "\n")
